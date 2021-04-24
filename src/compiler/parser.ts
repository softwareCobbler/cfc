import {
    setDebug as setNodeFactoryDebug,
    CfTag, Node, NodeType, TagAttribute, NodeFlags, Terminal, Comment, TextSpan, NilTerminal,
    Conditional, ConditionalSubtype, FunctionParameter, FromTag, FunctionDefinition, CommentType,
    HashWrappedExpr, Assignee, Assignment, BinaryOperator, Parenthetical, UnaryOperator, BooleanLiteral,
    CallExpression, IndexedAccess, pushAccessElement, CallArgument, Identifier, SimpleStringLiteral, InterpolatedStringLiteral,
    NumericLiteral, DottedPath, ArrowFunctionDefinition, Statement, Block, Switch, NamedBlockFromBlock,
    SwitchCase,
    Do,
    While,
    Ternary,
    For,
    StructLiteralInitializerMember,
    StructLiteral,
    ArrayLiteralInitializerMember,
    ArrayLiteral,
    EmptyOrderedStructLiteral,
    IndexedAccessType,
    OrderedStructLiteral,
    ReturnStatement,
    Try,
    Catch,
    Finally,
    BreakStatement,
    ContinueStatement,
    mergeRanges, } from "./node";
import { SourceRange, Token, TokenType, ScannerMode, Scanner, TokenTypeUiString, TokenTypeUiStringReverse } from "./scanner";
import { allowTagBody, isLexemeLikeToken, requiresEndTag, getAttributeValue, getTriviallyComputableBoolean, getTriviallyComputableString, isNamedBlockName } from "./utils";

const enum ParseOptions {
    none     = 0,
    noTrivia = 0,
    withTrivia        = 0x00000001,
    allowHashWrapped  = 0x00000002,
};

const enum ParseContext {
    none = 0,
    hashWrappedExpr   = 0x00000001, // in #...# in an expression context, like `a + #b#`
    cfScriptTagBody   = 0x00000002, // in a <cfscript> block
    for               = 0x00000004, // in a for (...) expression
    interpolatedText  = 0x00000008, // in <cfoutput>#...#</cfoutput> or "#...#"
    awaitingVoidSlash = 0x00000010, // <cfset foo = bar /> is just `foo=bar`, with a trailing tag-void-slash, not `foo=bar/` with a missing rhs to the `/` operator
    switch            = 0x00000020, // in a switch statement
}

function TagContext() {
    return {
        depth: {
            output: 0,
            mail: 0,
            query: 0
        },
        inTextInterpolationContext() {
            return this.depth.output > 0
                || this.depth.mail > 0
                || this.depth.query > 0;
        }
    }
}

type TagContext = ReturnType<typeof TagContext>;

interface TokenizerState {
    mode : ScannerMode;
    index: number;
    artificialEndLimit: number | undefined;
}

export interface Diagnostic {
    fromInclusive: number;
    toExclusive: number;
    msg: string;
}

export const enum CfFileType { cfm, cfc };

export function Parser() {
    function setScanner(scanner_: Scanner) {
        scanner = scanner_;
        parseContext = ParseContext.none;
        diagnostics = [];
        return self_;
    }
    function primeScanner() {
        lookahead_ = peek().type;
    }
    function setScannerMode(mode_: ScannerMode) {
        mode = mode_;
        primeScanner();
        return self_;
    }
    function setDebug(isDebug: boolean) {
        setNodeFactoryDebug(isDebug);
        return self_;
    }

    let scanner : Scanner;
    let mode: ScannerMode;
    let parseContext : ParseContext;
    let lookahead_ : TokenType;

    const identifierPattern = /^[_$a-z][_$a-z0-9]*$/i;
    const tagNamePattern    = /^[_a-z]+$/i;
    
    let globalDiagnosticEmitter : (() => void) | null = null; // @fixme: find a way to not need this

    let diagnostics : Diagnostic[] = [];

    const SpeculationHelper = (function() {
        //
        // run a boolean returning worker, and always rollback changes to parser state when done
        //
        function lookahead(lookaheadWorker: () => boolean) {
            const saveTokenizerState = getTokenizerState();
            const diagnosticsLimit = diagnostics.length;

            const result = lookaheadWorker();

            diagnostics.splice(diagnosticsLimit); // drop any diagnostics that were added
            restoreTokenizerState(saveTokenizerState);
            return result;
        }
        //
        // if speculationWorker returns a truthy `T`, we return that;
        // otherwise, rollback any changes to parser state made by the speculation worker and return null
        //
        function speculate<
            F extends (...args: any) => any,
            Args extends Parameters<F> = Parameters<F>>(speculationWorker: F, ...args: Args) : ReturnType<F> | null {
            const saveTokenizerState = getTokenizerState();
            const savedGlobalDiagnosticEmitter = globalDiagnosticEmitter;
            const diagnosticsLimit = diagnostics.length;

            const result = speculationWorker(...args as [...Args]);

            if (result) {
                return result;
            }
            else {
                restoreTokenizerState(saveTokenizerState);
                globalDiagnosticEmitter = savedGlobalDiagnosticEmitter;
                diagnostics.splice(diagnosticsLimit); // drop any diagnostics that were added
                return null;
            }
        }
        return {
            lookahead,
            speculate
        }
    })();

    const self_ = {
        setScanner,
        setScannerMode,
        setDebug,
        getDiagnostics,
        parseTags,
        parseScript,
        parse,
    };

    return self_;

    /*********************************
    /* impl
    /********************************/
    function peek(jump: number = 0) {
        return scanner.peek(jump, mode);
    }

    function lookahead() {
        return lookahead_;
    }

    function next() {
        const result = scanner.next(mode);
        lookahead_ = peek().type;
        return result;
    }

    function scanToNextToken(token: TokenType[], endOnOrAfter: "on" | "after" = "on") : void {
        scanner.scanToNext(token, mode);
        if (endOnOrAfter === "after") {
            scanner.next(mode);
        }
        lookahead_ = peek().type;
    }

    function scanToNextChar(char: string, endOnOrAfter: "on" | "after" = "on") : void {
        scanner.scanToNext(char);
        if (endOnOrAfter === "after") {
            scanner.advance();
        }
        lookahead_ = peek().type;
    }

    function getIndex() {
        return scanner.getIndex();
    }

    function getTokenizerState() : TokenizerState {
        return {
            index: scanner.getIndex(),
            mode: mode,
            artificialEndLimit: scanner.getArtificalEndLimit()
        }
    }

    function restoreTokenizerState(state: TokenizerState) {
        scanner.restoreIndex(state.index);
        mode = state.mode;
        if (state.artificialEndLimit) {
            scanner.setArtificialEndLimit(state.artificialEndLimit);
        }
        else {
            scanner.clearArtificalEndLimit();
        }
        lookahead_ = peek().type;
    }

    function tagMode() : boolean {
        return mode === ScannerMode.tag;
    }
    function scriptMode() : boolean {
        return mode === ScannerMode.script;
    }
    function isInSomeContext(contextFlags: ParseContext) {
        return (parseContext & contextFlags) === contextFlags;
    }

    //
    // run some function inside of some context
    // this totally erases the existing context for the duration of the body
    //
    function doInContext<T>(context: ParseContext, f: (() => T)) : T {
        const savedContext = parseContext;
        parseContext = context;
        const result = f();
        parseContext = savedContext;
        return result;
    }

    function parseErrorAtRange(range: SourceRange, msg: string) : void;
    function parseErrorAtRange(fromInclusive: number, toExclusive: number, msg: string) : void;
    function parseErrorAtRange(fromInclusiveOrRange: number | SourceRange, toExclusiveOrMsg: number | string, msg?: string) : void {
        let from : number;
        let to : number;
        if (typeof fromInclusiveOrRange === "number") {
            from = fromInclusiveOrRange;
            to = toExclusiveOrMsg as number;
        }
        else {
            from = fromInclusiveOrRange.fromInclusive;
            to = fromInclusiveOrRange.toExclusive;
        }

        const lastDiagnostic = diagnostics.length > 0 ? diagnostics[diagnostics.length-1] : undefined;
        const freshDiagnostic : Diagnostic = {
            fromInclusive: from,
            toExclusive: to,
            msg: msg ?? (toExclusiveOrMsg as string)
        };

        // this will break with current speculation logic
        // where during speculation we continue to emit diagnostics,
        // and if the speculation fails we drop any new diagnostics
        // if all we did was overwrite an old diagnostic, it won't get dropped
        if (lastDiagnostic) {
            if (lastDiagnostic.fromInclusive === from) {
                // last diagnostic started where this one starts, and is exactly as long or longer
                if (lastDiagnostic.toExclusive >= to) {
                    // no-op
                    return;
                }
                else {
                    // the new diagnostic starts where the last one starts, but is longer
                    // overwrite the older diagnostic
                    diagnostics[diagnostics.length-1] = freshDiagnostic;
                    return;
                }
            }
        }

        // default behavior, just push the new diagnostic
        diagnostics.push(freshDiagnostic);
        return;
    }

    function parseErrorAtCurrentToken(msg: string) : void {
        parseErrorAtRange(scanner.currentToken().range, msg);
    }

    function createMissingNode<T extends Node>(node: T) {
        node.flags |= NodeFlags.error | NodeFlags.missing;
        return node;
    }

    function parseOptionalTerminal(type: TokenType, parseOptions: ParseOptions) : Terminal | null {
        if (lookahead() === type) {
            const token = next();
            if (parseOptions & ParseOptions.withTrivia) {
                return Terminal(token, parseTrivia());
            }
            else {
                return Terminal(token);
            }
        }
        else {
            return null;
        }
    }

    function parseExpectedTerminal(type: TokenType, parseOptions: ParseOptions, localDiagnosticEmitter?: (() => void) | string) : Terminal {
        const maybeTerminal = parseOptionalTerminal(type, parseOptions);
        if (maybeTerminal) {
            return maybeTerminal;
        }
        else {
            if (localDiagnosticEmitter) {
                if (typeof localDiagnosticEmitter === "string") {
                    parseErrorAtCurrentToken(localDiagnosticEmitter);
                }
                else {
                    localDiagnosticEmitter();
                }
            }
            else if (globalDiagnosticEmitter) {
                globalDiagnosticEmitter();
            }
            else {
                const currentToken = scanner.currentToken();
                parseErrorAtRange(
                    currentToken.range.fromInclusive,
                    currentToken.range.fromInclusive+1,
                    "Expected a " + TokenTypeUiString[type] + " here");
            }

            const phonyToken : Token = Token(type, "", SourceRange.Nil());
            return createMissingNode(Terminal(phonyToken));
        }
    }

    // tag names like "function" (any others?) are also keywords
    // if they were only keywords in script mode, we wouldn't have to worry about it,
    // but they are also keywords in tag mode, e.g, `<cfset x = function() { ... }>`
    // and it is also a tagname; so when parsing a tag name we may not get a lexeme, but instead a keyword,
    // which `parseExpectedTerminal` is not equipped to handle (it expects to parse exactly one type of token per call)
    // so here, we don't care about the token type and instead just parse whatever follows as though it were a lexeme
    //
    // <cfswitch> is similar, but is not a keyword in tag mode so it could still be matched with `parseExpectedTerminal` looking for a lexeme;
    // <cftry> as well, and etc; really <cffunction> is the big offender
    //
    function parseExpectedTagName() {
        const token = next();
        if (!isLexemeLikeToken(token)) {
            parseErrorAtRange(token.range, "Expected a cftag name here");
        }
        else if (!tagNamePattern.test(token.text)) {
            parseErrorAtRange(token.range, "Invalid cftag name");
        }

        const trivia = parseTrivia();
        const forcedLexemeToken = Token(TokenType.LEXEME, token.text, token.range.fromInclusive, token.range.toExclusive);
        return Terminal(forcedLexemeToken, trivia);
    }

    function parseExpectedLexemeLikeTerminal(consumeOnFailure: boolean, allowNumeric: boolean, errorMsg?: string) : Terminal {
        const labelLike = peek();
        let trivia : Node[] = [];
        if (!isLexemeLikeToken(labelLike, allowNumeric)) {
            if (errorMsg) {
                parseErrorAtRange(labelLike.range, errorMsg);
            }
            else {
                if (globalDiagnosticEmitter) globalDiagnosticEmitter();
                else parseErrorAtRange(labelLike.range, "Expected a lexeme-like token here");
            }
            
            if (consumeOnFailure) {
                next();
                trivia = parseTrivia();
            }
        }
        else {
            next();
            trivia = parseTrivia();
        }
        const forcedLexemeToken = Token(TokenType.LEXEME, labelLike.text, labelLike.range.fromInclusive, labelLike.range.toExclusive);
        return Terminal(forcedLexemeToken, trivia);
    }

    function parseTagComment() : CfTag.Comment {
        const commentStart = parseExpectedTerminal(TokenType.CF_TAG_COMMENT_START, ParseOptions.noTrivia);
        const nestedComments : CfTag.Comment[] = [];
        while (true) {
            scanToNextToken([TokenType.CF_TAG_COMMENT_START, TokenType.CF_TAG_COMMENT_END]);
            if (lookahead() === TokenType.CF_TAG_COMMENT_START) {
                nestedComments.push(parseTagComment());
            }
            else {
                break;
            }
        }

        const commentEnd = parseExpectedTerminal(
            TokenType.CF_TAG_COMMENT_END,
            ParseOptions.noTrivia,
            () => parseErrorAtRange(commentStart.range.fromInclusive, scanner.getIndex(), "Unterminated tag comment"));
        return CfTag.Comment(commentStart, nestedComments, commentEnd);
    }

    function parseScriptSingleLineComment() : Comment {
        const start = parseExpectedTerminal(TokenType.DBL_FORWARD_SLASH, ParseOptions.noTrivia);
        scanToNextChar("\n", /*endOnOrAfter*/"after");
        return Comment(CommentType.scriptSingleLine, new SourceRange(start.range.fromInclusive, scanner.getIndex()));
    }

    function parseScriptMultiLineComment() : Comment {
        const startToken = parseExpectedTerminal(TokenType.FORWARD_SLASH_STAR, ParseOptions.noTrivia);
        scanToNextToken([TokenType.STAR_FORWARD_SLASH]);
        const endToken = parseExpectedTerminal(TokenType.STAR_FORWARD_SLASH, ParseOptions.noTrivia, () => parseErrorAtRange(startToken.range.fromInclusive, scanner.getIndex(), "Unterminated multiline script comment"));
        return Comment(CommentType.scriptMultiLine, new SourceRange(startToken.range.fromInclusive, endToken.range.toExclusive));
    }

    function parseTrivia() : Node[] {
        if (tagMode()) {
            return parseTagTrivia();
        }

        const result : Node[] = [];
        while (true) {
            switch (lookahead()) {
                case TokenType.DBL_FORWARD_SLASH:
                    result.push(parseScriptSingleLineComment());
                    continue;
                case TokenType.FORWARD_SLASH_STAR:
                    result.push(parseScriptMultiLineComment());
                    continue;
                case TokenType.WHITESPACE:
                    result.push(TextSpan(next().range, "")); // not really any need to store the whitespace
                    continue;
            }
            break;
        }
        return result;
    }

    function parseTagTrivia() : CfTag[] {
        const result : CfTag[] = [];
        while (true) {
            switch (lookahead()) {
                case TokenType.CF_TAG_COMMENT_START: {
                    result.push(parseTagComment());
                    continue;
                }
                case TokenType.WHITESPACE: {
                    result.push(CfTag.Text(next().range));
                    continue;
                }
            }
            // if we didn't match tag comment start or whitespace, we're done
            break;
        }
        return result;
    }

    function parseCfStartTag() {
        const tagStart = parseExpectedTerminal(TokenType.CF_START_TAG_START, ParseOptions.noTrivia);
        const tagName = parseExpectedTagName();
        const canonicalName = tagName.token.text.toLowerCase();

        switch (canonicalName) {
            case "if":
            case "elseif": {
                const expr = parseExpression();
                const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
                return CfTag.ScriptLike(CfTag.Which.start, tagStart, tagName, null, rightAngle, canonicalName, expr);
            }
            case "set": {
                const expr = doInContext(ParseContext.awaitingVoidSlash, parseAssignmentOrLower);
                const maybeVoidSlash = parseOptionalTerminal(TokenType.FORWARD_SLASH, ParseOptions.withTrivia);
                const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
                return CfTag.ScriptLike(CfTag.Which.start, tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, expr);
            }
            case "return": {
                if (lookahead() === TokenType.FORWARD_SLASH || lookahead() === TokenType.RIGHT_ANGLE) {
                    const maybeVoidSlash = parseOptionalTerminal(TokenType.FORWARD_SLASH, ParseOptions.withTrivia);
                    const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
                    return CfTag.ScriptLike(CfTag.Which.start, tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, null);
                }
                else {
                    const expr = doInContext(ParseContext.awaitingVoidSlash, parseAnonymousFunctionDefinitionOrExpression);
                    const maybeVoidSlash = parseOptionalTerminal(TokenType.FORWARD_SLASH, ParseOptions.withTrivia);
                    const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
                    return CfTag.ScriptLike(CfTag.Which.start, tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, expr);
                }
            }
            default: {
                const tagAttrs = parseTagAttributes();
                const maybeVoidSlash = parseOptionalTerminal(TokenType.FORWARD_SLASH, ParseOptions.withTrivia);
                const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
                if (canonicalName === "script") {
                    if (tagAttrs.length > 0) {
                        parseErrorAtRange(mergeRanges(tagAttrs), "A <cfscript> tag cannot contain attributes.");
                    }
                    
                    const savedMode = mode;
                    setScannerMode(ScannerMode.script);

                    // hm, probably mode should be a context flag, too
                    rightAngle.trivia = parseTrivia(); // <cfscript> is a tag but it gets script-based trivia (so it can have script comments attached to it)
                    const stmtList = doInContext(ParseContext.cfScriptTagBody, parseStatementList);

                    setScannerMode(savedMode);
                    return CfTag.Script(tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, stmtList);
                }
                else {
                    return CfTag.Common(CfTag.Which.start, tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, tagAttrs);
                }
            }
        }
    }

    function parseCfEndTag() {
        const tagStart = parseExpectedTerminal(TokenType.CF_END_TAG_START, ParseOptions.noTrivia);
        const tagName = parseExpectedTagName();
        const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);

        let canonicalName = "";
        if (!(tagName.flags & NodeFlags.missing)) {
            canonicalName = scanner.getTokenText(tagName.token).toLowerCase();
        }
        return CfTag.Common(CfTag.Which.end, tagStart, tagName, null, rightAngle, canonicalName);
    }

    function parseTagAttributes() : TagAttribute[] {
        const result : TagAttribute[] = [];

        while (isLexemeLikeToken(peek(), /*allowNumeric*/ true)) {
            // @fixme: does this need "consumeOnFailure", does anyone use "true"?
            const attrName = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ false);
            if (lookahead() === TokenType.EQUAL) {
                const equal = parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
                let value : Node;

                if (isLexemeLikeToken(peek(), /*allowNumeric*/ true)) {
                    value = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ true);
                }
                else if (lookahead() === TokenType.QUOTE_SINGLE || lookahead() === TokenType.QUOTE_DOUBLE) {
                    value = parseStringLiteral();
                }
                else if (lookahead() === TokenType.HASH) {
                    value = parseHashWrappedExpression();
                }
                else {
                    parseErrorAtCurrentToken("Expected a tag-attribute value here.");
                    value = createMissingNode(NilTerminal);
                }

                result.push(TagAttribute(attrName, scanner.getTokenText(attrName.token).toLowerCase(), equal, value));
            }
            else {
                result.push(TagAttribute(attrName, scanner.getTokenText(attrName.token).toLowerCase()));
            }
        }

        return result;
    }

    function parseComponentPreamble() : "script" | "tag" | null {
        setScannerMode(ScannerMode.allow_both);
        const preamble : Node[] = [];

        //
        // speculationhelper will cleanup scannermode changes
        //
        function isTagComponentBlock() {
            setScannerMode(ScannerMode.tag);
            if (lookahead() !== TokenType.CF_START_TAG_START) {
                return false;
            }
            return parseCfStartTag().canonicalName === "component";
        }

        function isScriptComponentBlock() {
            setScannerMode(ScannerMode.script);
            if (peek().text.toLowerCase() !== "component") {
                return false;
            }
            next();
            parseTrivia();
            parseTagAttributes();
            return peek().type === TokenType.LEFT_BRACE;
        }

        let gotNonComment = false;
        let gotTagComment = false;
        let gotScriptComment = false;
        let match : "script" | "tag" | null = null;
        outer:
        while (lookahead() !== TokenType.EOF) {
            switch (lookahead()) {
                case TokenType.CF_TAG_COMMENT_START: {
                    preamble.push(parseTagComment());
                    gotTagComment = true;
                    continue;
                }
                case TokenType.DBL_FORWARD_SLASH: {
                    preamble.push(parseScriptSingleLineComment());
                    gotScriptComment = true;
                    continue;
                }
                case TokenType.FORWARD_SLASH_STAR: {
                    preamble.push(parseScriptMultiLineComment());
                    gotScriptComment = true;
                    continue;
                }
                case TokenType.LEXEME: {
                    if (SpeculationHelper.lookahead(isScriptComponentBlock)) {
                        match = "script";
                        break outer;
                    }
                    gotNonComment = true;
                    next();
                    continue;
                }
                case TokenType.CF_START_TAG_START: {
                    if (SpeculationHelper.lookahead(isTagComponentBlock)) {
                        match = "tag";
                        break outer;
                    }
                    gotNonComment = true;
                    next();
                    continue;
                }
                case TokenType.WHITESPACE: {
                    next();
                    continue;
                }
                default: {
                    gotNonComment = true;
                    next();
                    continue;
                }
            }
        }

        if (gotNonComment) {
            parseErrorAtRange(0, getIndex(), "A component preamble may only contain comments.");
        }

        if (match && ((match === "tag" && gotScriptComment) || (match === "script" && gotTagComment))) {
            parseErrorAtRange(0, getIndex(), `A ${match} component preamble may only contain ${match}-style comments.`);
        }
        else if (!match) {
            parseErrorAtRange(0, getIndex(), `A CFC file must contain a component definition.`);
        }

        return match;
    }

    function parse(cfFileType: CfFileType) : Node[] {
        if (cfFileType === CfFileType.cfm) {
            return parseTags();
        }

        const componentType = parseComponentPreamble();
        if (!componentType) {
            return [];
        }
        else if (componentType === "tag") {
            return parseTags();
        }
        else {
            return parseScript();
        }
    }

    function parseScript() : Node[] {
        setScannerMode(ScannerMode.script);
        return parseStatementList();
    }

    function parseTags() : Node[] {
        //
        // tag treeifier
        // after parsing tags, we end up with just a list of tags
        // we need to go through a second time and turn them into a tree
        // we convert text spans into interpolated text spans here if necessary
        // as well as convert all tags into either blocks or statements;
        // a tag with children becomes a block, a single tag (like a <cfset ...> or just a loose <cffile ...> ) becomes a statement
        // this also gives us the opportunity to emit diagnostics for unbalanced start or end tags
        //
        function treeifyTagList(tagList: CfTag[]) : Node[] {
            const tagContext = TagContext();
            const openTagStack : string[] = [];
            let index = 0;

            function openTagStackFindMatchingStartTag(tagCanonicalName: string) : number | null {
                for (let i = openTagStack.length-1; i >= 0; i--) {
                    if (openTagStack[i] === tagCanonicalName) return i;
                }
                return null;
            }

            const enum ReductionScheme { default, return };
            interface ReductionInstruction {
                onHitWhich: CfTag.Which,
                onHitName: string,
                reduction: ReductionScheme;
            }
            const reductionInstructions = {
                cfif: <readonly ReductionInstruction[]>[
                    { onHitWhich: CfTag.Which.start, onHitName: "elseif", reduction: ReductionScheme.return },
                    { onHitWhich: CfTag.Which.start, onHitName: "else",   reduction: ReductionScheme.return },
                    { onHitWhich: CfTag.Which.end,   onHitName: "if",     reduction: ReductionScheme.return }
                ],
                cfelseif: <readonly ReductionInstruction[]>[
                    { onHitWhich: CfTag.Which.start, onHitName: "elseif", reduction: ReductionScheme.return },
                    { onHitWhich: CfTag.Which.start, onHitName: "else",   reduction: ReductionScheme.return },
                    { onHitWhich: CfTag.Which.end,   onHitName: "if",     reduction: ReductionScheme.return }
                ],
                cfelse: <readonly ReductionInstruction[]>[
                    {onHitWhich: CfTag.Which.end, onHitName: "if", reduction: ReductionScheme.return }
                ],
                default: <readonly ReductionInstruction[]>[]
            };

            function updateTagContext(tag: CfTag) {
                let bumpDir = tag.which === CfTag.Which.start ? 1 : -1;
                switch (tag.canonicalName) {
                    case "output":
                    case "mail":
                    case "query": {
                        tagContext.depth[tag.canonicalName] += bumpDir;
                    }
                }
            }

            function stopAt(which: CfTag.Which, name: string, reduction: ReductionScheme) : ReductionInstruction[] {
                return [{
                    onHitWhich: which,
                    onHitName: name,
                    reduction: reduction
                }]
            }

            function getReductionScheme(tag: CfTag, instructions: readonly ReductionInstruction[]) : ReductionScheme {
                for (const instr of instructions) {
                    if (instr.onHitWhich === tag.which && instr.onHitName === tag.canonicalName) {
                        return instr.reduction;
                    }
                }
                return ReductionScheme.default;
            }

            function hasNextTag() : boolean {
                return index < tagList.length;
            }

            // @ts-expect-error - unused
            function next() : never { // shadow the outer next()
                throw "use nextTag";
            }
            
            function nextTag() {
                return tagList[index++];
            }
            function peekTag() {
                return hasNextTag() ? tagList[index] : null;
            }

            function parseOptionalTag(which: CfTag.Which, canonicalName: string) : CfTag | null {
                if (hasNextTag() && tagList[index].which === which && tagList[index].canonicalName === canonicalName) {
                    return nextTag();
                }
                else {
                    return null;
                }
            }

            function parseExpectedTag(which: CfTag.Which, canonicalName: string, diagnosticEmitter?: () => void) : CfTag {
                const tag = parseOptionalTag(which, canonicalName);
                if (tag) {
                    return tag;
                }
                else {
                    if (diagnosticEmitter) {
                        diagnosticEmitter();
                    }
                    else {
                        const msg = `Expected a <${which === CfTag.Which.end ? "/" : ""}cf${canonicalName}> tag here`;
                        let range = hasNextTag() ? peekTag()!.range : tagList[tagList.length-1].range;
                        parseErrorAtRange(range, msg);
                    }

                    // create fake placeholder tag
                    let missingTag : CfTag.Common;
                    if (which === CfTag.Which.start) {
                        missingTag = CfTag.Common(which, NilTerminal, NilTerminal, null, NilTerminal, canonicalName, [])
                    }
                    else {
                        missingTag = CfTag.Common(which, NilTerminal, NilTerminal, null, NilTerminal, canonicalName)
                    }
                    createMissingNode(missingTag);
                    return missingTag;
                }
            }

            function treeifyConditionalTag() {
                const ifTag = parseExpectedTag(CfTag.Which.start, "if");
                openTagStack.push("if");
                const rootConsequent = treeifyTags(reductionInstructions.cfif);

                const consequentAsBlock = Block(null, rootConsequent, null);
                consequentAsBlock.tagOrigin.startTag = ifTag;

                let root = FromTag.Conditional(ConditionalSubtype.if, ifTag, consequentAsBlock);
                let working = root;

                while (true) {
                    const elseIfTag = parseOptionalTag(CfTag.Which.start, "elseif");
                    if (elseIfTag) {
                        const consequent = treeifyTags(reductionInstructions.cfelseif);
                        const consequentAsBlock = Block(null, consequent, null);
                        consequentAsBlock.tagOrigin.startTag = elseIfTag;
                        working.alternative = FromTag.Conditional(ConditionalSubtype.elseif, elseIfTag, consequentAsBlock);
                        working = root.alternative!;
                        continue;
                    }
                    const elseTag = parseOptionalTag(CfTag.Which.start, "else");
                    if (elseTag) {
                        const consequent = treeifyTags(reductionInstructions.cfelse);
                        const consequentAsBlock = Block(null, consequent, null);
                        consequentAsBlock.tagOrigin.startTag = elseTag;
                        working.alternative = FromTag.Conditional(ConditionalSubtype.else, elseTag, consequentAsBlock);
                    }
                    break;
                }

                openTagStack.pop();

                if (hasNextTag()) {
                    const nextTag = peekTag()!;
                    if (nextTag.canonicalName === "if" && nextTag.which === CfTag.Which.end) {
                        root.tagOrigin.endTag = parseExpectedTag(CfTag.Which.end, "if");
                        return root;
                    }
                }

                parseErrorAtRange(root.range, "Missing </cfif> tag.");
                return root;
            }

            function treeifyTagFunction(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) {
                function parseParam(tag: CfTag.Common) : FunctionParameter {
                    const nameAttr = getAttributeValue(tag.attrs, "name");
                    let name = "";
                    if (!nameAttr) {
                        parseErrorAtRange(tag.range, "<cfargument> requires a 'name' attribute.");
                    }
                    else {
                        const nameVal = getTriviallyComputableString(nameAttr);
                        if (!nameVal) {
                            parseErrorAtRange(nameAttr.range, "<cfargument> 'name' attribute must be a constant string value.");
                        }
                        else {
                            name = nameVal;
                        }
                    }

                    const requiredAttr = getAttributeValue(tag.attrs, "required");
                    let isRequired = false;
                    if (requiredAttr) {
                        if (parseFloat(getTriviallyComputableString(requiredAttr)!) < 0) {
                            // parseFloat does accept undefined, but the TS signature is overly-safe
                            // so we might get a NaN out of it, but that's ok because NaN comparisons always result in false
                            // if we don't get a NaN, and we are less than 0, report an error, because there is a special rule,
                            // or more of a actual cf-engine parse-failure, that negative numbers cannot be coerced to boolean
                            // in the requires attribute of a <cfargument> tag
                            parseErrorAtRange(requiredAttr.range, "<cfargument> 'required' attribute does not allow coercions to boolean from negative numbers.");
                            isRequired = false;
                        }
                        else {
                            const boolVal = getTriviallyComputableBoolean(requiredAttr);
                            if (boolVal === undefined) { // whatever the value was, it was not coercible to bool
                                parseErrorAtRange(requiredAttr.range, "<cfargument> 'required' attribute must be a constant boolean value.");
                            }
                            isRequired = boolVal ?? false;
                        }
                    }

                    return FromTag.FunctionParameter(tag, name, isRequired, /*FIXME*/ null);
                }

                let functionName = "";
                const functionNameExpr = getAttributeValue(startTag.attrs, "name");
                if (!functionNameExpr) {
                    parseErrorAtRange(startTag.range, "<cffunction> requires a name attribute.")
                }
                else {
                    let functionName = getTriviallyComputableString(functionNameExpr);
                    if (!functionName) {
                        parseErrorAtRange(functionNameExpr.range, "<cffunction> name attribute must be a constant.")
                    }
                }

                const params : FunctionParameter[] = [];
                let i = 0;
                for (; i < body.length; i++) {
                    const node = body[i];
                    if (node.type === NodeType.textSpan || node.type === NodeType.comment) {
                        continue;
                    }
                    if (node.type === NodeType.tag && node.canonicalName === "argument") {
                        params.push(parseParam(node as CfTag.Common));
                        continue;
                    }
                    break;
                }

                body = body.splice(i); // drop all the params and whitespace that we consumed as FunctionParameters
                return FromTag.FunctionDefinition(
                    startTag,
                    params,
                    Block(null, body, null), // there are no braces on a tag-originating function
                    endTag,
                    functionName);
            }

            function treeifyTags(reductionInstructions: readonly ReductionInstruction[]) : Node[] {
                const result : Node[] = [];

                function localStackFindMatchingStartTag(tag: CfTag) : number | null {
                    for (let i = result.length - 1; i >= 0; i--) {
                        if (result[i].type === NodeType.tag) {
                            const stackTag = result[i] as CfTag;
                            if (stackTag.which === CfTag.Which.end && stackTag.canonicalName === tag.canonicalName) {
                                return i;
                            }
                        }
                    }
                    return null;
                }

                // handle a tag block that requires a matching start/end tag pair
                function tagBlockWorker(tag: CfTag) {
                    openTagStack.push(tag.canonicalName);

                    const startTag = tag;
                    nextTag();
                    const blockChildren = treeifyTags(stopAt(CfTag.Which.end, startTag.canonicalName, ReductionScheme.return));

                    openTagStack.pop();

                    const endTag = parseExpectedTag(CfTag.Which.end, startTag.canonicalName, () => parseErrorAtRange(startTag.range, "Missing </cf" + startTag.canonicalName + "> tag."));
                    updateTagContext(endTag);
                    result.push(FromTag.Block(startTag, blockChildren, endTag));
                }

                while (hasNextTag()) {
                    const tag = peekTag()!;
                    updateTagContext(tag);

                    if (tag.tagType === CfTag.TagType.text) {
                        const text = parseTextInTagContext(tag.range, tagContext);

                        if (Array.isArray(text)) {
                            result.push(...text);
                        }
                        else {
                            result.push(text);
                        }

                        nextTag()
                        continue;
                    }
                    else if (tag.tagType === CfTag.TagType.comment) {
                        result.push(Comment(CommentType.tag, tag.range));
                        nextTag();
                        continue;
                    }
                    else if (tag.which === CfTag.Which.start) {
                        const reductionScheme = getReductionScheme(tag, reductionInstructions);
                        switch (reductionScheme) {
                            case ReductionScheme.return:
                                return result;
                            case ReductionScheme.default:
                                // ok, no-op: the default reduction scheme for a start tag is to do nothing
                                break;
                        }

                        switch (tag.canonicalName) {
                            case "if": {
                                result.push(treeifyConditionalTag());
                                continue;
                            }
                            case "set": {
                                result.push(FromTag.Statement(<CfTag.ScriptLike>tag));
                                nextTag();
                                continue;
                            }
                            case "return": {
                                result.push(FromTag.ReturnStatement(<CfTag.ScriptLike>tag));
                                nextTag();
                                continue;
                            }
                            case "function": {
                                openTagStack.push("function");
                                const startTag = parseExpectedTag(CfTag.Which.start, "function");
                                const body = treeifyTags(stopAt(CfTag.Which.end, "function", ReductionScheme.return));
                                const endTag = parseExpectedTag(CfTag.Which.end, "function", () => parseErrorAtRange(startTag.range, "Missing </cffunction> tag."))
                                openTagStack.pop();
                                result.push(treeifyTagFunction(startTag as CfTag.Common, body, endTag as CfTag.Common));
                                continue;
                            }
                            case "transaction": {
                                const actionAttrValue = getAttributeValue((<CfTag.Common>tag).attrs, "action");
                                // there is NO action attribute, so it is implied to be "action=begin", which requires a body
                                if (!actionAttrValue) {
                                    if (tag.voidSlash) parseErrorAtRange(tag.voidSlash.range, "Unexpected '/' in <cftransaction> block with implied action=begin attribute.");
                                    tagBlockWorker(tag);
                                    continue;
                                }

                                const valueAsString = getTriviallyComputableString(actionAttrValue)?.toLowerCase();
                                if (valueAsString === "begin") {
                                    if (tag.voidSlash) {
                                        if (valueAsString === "begin")
                                            parseErrorAtRange(tag.voidSlash.range, "Unexpected '/' in <cftransaction action=begin> block.");
                                        else
                                            parseErrorAtRange(tag.voidSlash.range, "Unexpected '/' in <cftransaction> block with implied action=begin attribute.");
                                    }
                                    tagBlockWorker(tag);
                                }
                                else if (valueAsString === "commit" || valueAsString === "rollback" || valueAsString === "setsavepoint") {
                                    result.push(
                                        FromTag.Block(/*startTag*/ tag, /*stmtList*/ [], /*endTag*/ null));
                                    nextTag();
                                }
                                else {
                                    // <cftransaction> allows for a dynamic "action" attribute, e.g, `<cftransaction action="#dont_know_until_runtime#">`
                                    // so best we can do is honor the possibly-present void slash
                                    // and if the value is a static string, here we know it is an invalid one
                                    if (typeof valueAsString === "string") {
                                        parseErrorAtRange(actionAttrValue.range, "Unsupported <cftransaction> 'action' attribute.");
                                    }
                                    if (tag.voidSlash) {
                                        result.push(
                                            FromTag.Block(tag, [], null));
                                        nextTag();
                                    }
                                    else {
                                        tagBlockWorker(tag);
                                    }
                                }

                                continue;
                            }
                            case "script": {
                                nextTag();
                                const endTag = parseExpectedTag(CfTag.Which.end, "script", () => parseErrorAtRange(tag.range, "Missing </cfscript> tag."));
                                result.push(
                                    FromTag.Block(
                                        tag,
                                        (<CfTag.Script>tag).stmtList,
                                        endTag));
                                continue;
                            }
                            default: {
                                if (requiresEndTag(tag)) {
                                    tagBlockWorker(tag);
                                }
                                else {
                                    // a single loose tag
                                    // it will become a statement
                                    // e.g., <cfhttp args /> is essentially a call to a fictitious "cfhttp(args)" function, except it is a statement instead of a value producing expression
                                    // but first we push it into the result as a tag, so that any possible matching end tags can walk up the local results list
                                    // and find it 
                                    result.push(tag);
                                    nextTag();
                                }
                            }
                        }
                    }
                    else if (tag.which === CfTag.Which.end) {
                        const reductionScheme = getReductionScheme(tag, reductionInstructions);
                        if (reductionScheme === ReductionScheme.return) {
                                return result;
                        }

                        // if we can't find the target tag in our local result, this tag has no matching start tag
                        // if this tag isn't allowed to have a block body, it's an error, and gets discarded
                        if (!allowTagBody(tag)) {
                            // parse error - end tag for tag type that does not support tag bodys (like cfargument)
                            nextTag();
                            continue;
                        }

                        const maybeMatchingStartTagIndex = localStackFindMatchingStartTag(tag);
                        if (maybeMatchingStartTagIndex) {
                            const matchingStartTagIndex = maybeMatchingStartTagIndex;
                            const matchingStartTag = result[matchingStartTagIndex] as CfTag;

                            //
                            // move (matching_tag + 0) to be the head of a new block
                            // move everything from (matching_tag+1) into the new block as children
                            // use current tag as end of tag block
                            // once everything has been moved into their new homes, drop the original references
                            // from our local results list
                            //
					        
                            const blockChildren = result.slice(matchingStartTagIndex + 1);
                            result.splice(matchingStartTagIndex)
                            result.push(FromTag.Block(matchingStartTag, blockChildren, tag));
                        }
                        else {
                            // this tag might be a mismatched tag,
                            // in which case we return the current results, but do not consume the current tag
                            // this will naturally result in an "unmatched tag" error in the caller
                            const matchingOpenTagIndex = openTagStackFindMatchingStartTag(tag.canonicalName);
                            if (matchingOpenTagIndex !== null) {
                                return result;
                            }
                            else {
                                parseErrorAtRange(tag.range, "End tag without a matching start tag (cf" + tag.canonicalName + ").")
                            }
                        }

                        nextTag();
                    }
                }

                return result;
            }

            return treeifyTags(reductionInstructions.default);
        }

        const savedMode = mode;
        setScannerMode(ScannerMode.tag);

        const result : CfTag[] = [];
        let tagTextRange = SourceRange.Nil();

        function startOrContinueTagTextRange() {
            if (tagTextRange.isNil()) {
                const index = scanner.getIndex();
                tagTextRange = new SourceRange(index, index+1);
            }
        }
        function finishTagTextRange() {
            if (tagTextRange.isNil()) {
                return;
            }
            tagTextRange.toExclusive = scanner.getIndex(); // does not include current; so, no "+1"
            result.push(CfTag.Text(tagTextRange))
            tagTextRange = SourceRange.Nil();
        }

        while (lookahead() != TokenType.EOF) {
            switch (lookahead()) {
                case TokenType.CF_START_TAG_START: {
                    finishTagTextRange();
                    result.push(parseCfStartTag());
                    break;
                }
                case TokenType.CF_END_TAG_START: {
                    finishTagTextRange();
                    result.push(parseCfEndTag());
                    break;
                }
                case TokenType.CF_TAG_COMMENT_START: {
                    finishTagTextRange();
                    result.push(parseTagComment());
                    break;
                }
                default: {
                    startOrContinueTagTextRange();
                    next();
                }
            }
        }

        setScannerMode(savedMode);

        return treeifyTagList(result);
    }

    function parseTextInTagContext(range: SourceRange, tagContext: TagContext) {
        if (!tagContext.inTextInterpolationContext()) {
            return TextSpan(range, scanner.getTextSlice(range));
        }
        const saveTokenizerState = getTokenizerState();
        restoreTokenizerState({
            index: range.fromInclusive,
            mode: ScannerMode.tag,
            artificialEndLimit: range.toExclusive});
        
        const result = parseInterpolatedText();
        restoreTokenizerState(saveTokenizerState);
        return result;
    }

    function parseInterpolatedText(quoteDelimiter?: TokenType.QUOTE_SINGLE | TokenType.QUOTE_DOUBLE) : (TextSpan | HashWrappedExpr)[] {
        // when we enter a new interpolation context, we can reset our hashWrappedExpr flag;
        // hash wrapped expressions don't nest directly, but indirectly is ok; e.g,
        // #someVal & "some-string-literal #nesting_here_is_ok#" & trying_to_nest_here_is_an_error #
        const savedContext = parseContext;
        parseContext &= ~ParseContext.hashWrappedExpr;
        parseContext |= ParseContext.interpolatedText;

        const result : (TextSpan | HashWrappedExpr)[] = [];
        let textSourceRange = SourceRange.Nil();

        function startOrContinueTextRange() {
            if (textSourceRange.isNil()) {
                const index = scanner.getIndex();
                textSourceRange = new SourceRange(index, index+1);
            }
            // continuing is a no-op; when we finish the text range, we'll update the "toExclusive"
            // with the tokenizer's current index
        }
        function finishTextRange() {
            // if we hadn't started a range yet, we're done
            if (textSourceRange.isNil()) {
                return;
            }
            textSourceRange.toExclusive = scanner.getIndex(); // current index is NOT included, so, no '+1'
            result.push(TextSpan(textSourceRange, scanner.getTextSlice(textSourceRange)));
            textSourceRange = SourceRange.Nil();
        }

        //
        // there is no anchor to stop at other than EOF if the delimiter undefined
        // we rely on the caller setting the tokenizer to have an artifical range limit, such that we
        // eat only some pre-determined range of what we consider to be valid text
        //
        while (lookahead() != TokenType.EOF) {
            switch (lookahead()) {
                case quoteDelimiter: { // doubled-up delimiter; meaning it is an escaped quote
                    if (peek(1).type === quoteDelimiter) {
                        startOrContinueTextRange();
                        next(), next();
                        continue;
                    }
                    else { // single delimiter; we're done, don't eat it because the caller will need it
                        finishTextRange();
                        return result;
                    }
                }
                case TokenType.HASH: {
                    if (peek(1).type === TokenType.HASH) { // doubled up hash, meaning it is an escaped hash symbol
                        startOrContinueTextRange();
                        next(), next();
                        continue;
                    }
                    else { // single hash, meaning this is an interpolated string element
                        finishTextRange();
                        result.push(parseHashWrappedExpression());
                        continue;
                    }
                }
                default: {
                    startOrContinueTextRange();
                    next();
                }
            }
        }
        
        finishTextRange();
        parseContext = savedContext;
        return result;
    }

    function isAssignmentTarget(node: Node) : boolean {
        switch (node.type) {
            case NodeType.indexedAccess:
            case NodeType.identifier:
            case NodeType.simpleStringLiteral:          // <cfset "x" = "y">
            case NodeType.interpolatedStringLiteral:    // <cfset "#x#" = 42> <!--- same as <cfset y = 42>
                return true;
            case NodeType.hashWrappedExpr:
                return isAssignmentTarget(node.expr);
            default:
                return false;
        }
    }

    function parseAssignmentOrLower() : Node {
        const finalModifier = parseOptionalTerminal(TokenType.KW_FINAL, ParseOptions.withTrivia);
        const varModifier = parseOptionalTerminal(TokenType.KW_VAR, ParseOptions.withTrivia);
        const root = parseCallExpressionOrLower();

        if (lookahead() != TokenType.EQUAL) {
            if (varModifier) {
                // we could probably return a "declaration" here ?
                // var x;  sure it's semantically meaningless in cf (all var declarations require initializers), but it looks valid
                // would also be useful in a for-in: `for (var x in y)` where `var x` is a declaration
                // so we could check if we're in a for-in context
                parseErrorAtRange(root.range, "Variable declarations require initializers.");
            }
            return root;
        }

        if (!isAssignmentTarget(root)) {
            parseErrorAtRange(root.range, "left-hand side of assignment is not a valid assignment target");
        }

        const assignmentChain : Assignee[] = [];
        do {
            assignmentChain.push({
                equals: parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia),
                value: parseAnonymousFunctionDefinitionOrExpression()
            });
        } while (lookahead() === TokenType.EQUAL && isAssignmentTarget(assignmentChain[assignmentChain.length-1].value));

        return Assignment(finalModifier, varModifier, root, assignmentChain);
    }

    function parseAnonymousFunctionDefinitionOrExpression() : Node {
        if (lookahead() === TokenType.KW_FUNCTION) {
            return parseAnonymousFunctionDefinition();
        }

        const maybeArrowFunction = SpeculationHelper.speculate(tryParseArrowFunctionDefinition);
        if (maybeArrowFunction) {
            return maybeArrowFunction;
        }

        return parseExpression();
    }

    function parseExpression(descendIntoOr = true) : Node { // todo: does this get the AND and OR precedences correct?
        const saveDiagnosticEmitter = globalDiagnosticEmitter;
        const currentPos = scanner.getIndex();
        globalDiagnosticEmitter = () => parseErrorAtRange(currentPos, scanner.getIndex(), "Expression expected.");

        let root = parseComparisonExpressionOrLower();

        outer:
        while (true) {
            switch (lookahead()) {
                case TokenType.DBL_PIPE:
                case TokenType.LIT_OR:
                case TokenType.LIT_XOR: {
                    if (!descendIntoOr) break outer;
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseComparisonExpressionOrLower();
                    root = BinaryOperator(root, op, expr);
                    continue;
                }
                case TokenType.DBL_AMPERSAND:
                case TokenType.LIT_AND: {
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseExpression(/*descendIntoOr*/ false);
                    root = BinaryOperator(root, op, expr);
                    continue;
                }
                case TokenType.QUESTION_MARK: {
                    const questionMark = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);

                    //
                    // the null coalescing ("elvis") operator does not appear to actually be a token; the following is valid in cf2018+
                    // `v ? /*comment*/ : 0`, which means the same as `v ?: 0`
                    // and in tag mode `v ? <!------> : 0`
                    // so if we matched a "?", followed by trivia, followed by ":", we got a null coalescing operator
                    // otherwise, we got a ternary operator
                    //

                    if (lookahead() === TokenType.COLON) {
                        const colon = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                        if (questionMark.range.toExclusive !== colon.range.fromInclusive) {
                            // @fixme: make this a warning
                            parseErrorAtRange(questionMark.range.fromInclusive, colon.range.toExclusive, "Consider treating the null coalescing operator as a single token");
                        }

                        const syntheticOp = Terminal(Token(TokenType.QUESTION_MARK_COLON, "?:", questionMark.range.fromInclusive, colon.range.toExclusive), colon.trivia);
                        const right = parseExpression();
                        root = BinaryOperator(root, syntheticOp, right);
                    }
                    else {
                        const ternaryTrue = parseExpression();
                        const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                        const ternaryFalse = parseExpression();
                        root = Ternary(root, questionMark, ternaryTrue, colon, ternaryFalse);
                    }
                    continue;
                }
            }
            // if we didn't match any of the above operators, we're done
            break;
        }

        globalDiagnosticEmitter = saveDiagnosticEmitter;
        return root;
    }

    function parseComparisonExpressionOrLower() : Node {
        let root = parseAddition();

        while (true) {
            if (tagMode() && (lookahead() === TokenType.LEFT_ANGLE || lookahead() === TokenType.RIGHT_ANGLE)) {
                break;
            }
            switch (lookahead()) {
                case TokenType.DBL_EQUAL:      		// [[fallthrough]];
                case TokenType.LIT_EQ:        		// [[fallthrough]];
                case TokenType.LIT_IS:              // [[fallthrough]];
                case TokenType.EXCLAMATION_EQUAL:	// [[fallthrough]];
                case TokenType.LIT_IS_NOT:          // [[fallthrough]];
                case TokenType.LIT_NEQ:           	// [[fallthrough]];
    
                case TokenType.LEFT_ANGLE:    		// [[fallthrough]]; // invalid in tag mode, but we've already checked for it
                case TokenType.LIT_LT:              // [[fallthrough]];
                case TokenType.LEFT_ANGLE_EQUAL: 	// [[fallthrough]];
                case TokenType.LIT_LTE:				// [[fallthrough]];
                case TokenType.LIT_LE:				// [[fallthrough]];
    
                case TokenType.RIGHT_ANGLE:    		// [[fallthrough]]; // invalid in tag mode, but we've already checked for it
                case TokenType.LIT_GT:				// [[fallthrough]];
                case TokenType.RIGHT_ANGLE_EQUAL: 	// [[fallthrough]];
                case TokenType.LIT_GTE:				// [[fallthrough]];
                case TokenType.LIT_GE: {
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const right = parseAddition();
                    root = BinaryOperator(root, op, right);
                    continue;
                }
            }

            // if we didn't match any of the above tokens, we're done
            break;
        }

        return root;
    }

    function parseAddition() {
        let root = parseMultiplication();

        while (true) {
            switch (lookahead()) {
                case TokenType.PLUS:
                case TokenType.MINUS:
                case TokenType.AMPERSAND: {
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseMultiplication();
                    root = BinaryOperator(root, op, expr);
                    continue;
                }
            }
            // if we didn't match any of the above operators, we're done
            break;
        }

        return root;
    }

    function parseMultiplication() : Node {
        const stack : Node[] = [];

        // bind "^" (exponentiation) right
        function reduceRight() : void {
            if (stack.length === 1) return;

            // the stack should always have an odd number of elements,
            // (expr (op expr (op expr ...) ...) ...)
            while (stack.length > 1 && (stack[stack.length-2] as Terminal).token.type === TokenType.CARET) {
                const reduced = BinaryOperator(
                    stack[stack.length - 3],
                    stack[stack.length - 2] as Terminal,
                    stack[stack.length - 1]);
                stack.splice(stack.length - 3, 3);
                stack.push(reduced);
            }
        }

        stack.push(parseParentheticalOrUnaryPrefix());

        outer:
        while (true) {
            switch (lookahead()) {
                case TokenType.FORWARD_SLASH: {
                    // if we're awaiting a void slash, check to see if there is no expression following the slash
                    // if no expression follows this, we found the void slash we were looking for
                    if (isInSomeContext(ParseContext.awaitingVoidSlash)) {
                        const slashIsNotFollowedByExpression = SpeculationHelper.lookahead(function() {
                            return next(), parseTrivia(), !isStartOfExpression();
                        });
                        if (slashIsNotFollowedByExpression) {
                            reduceRight();
                            break outer;
                        }
                    }
                    // fallthrough
                }
                case TokenType.STAR:
                case TokenType.LIT_MOD: {
                    reduceRight();
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseParentheticalOrUnaryPrefix();
                    stack.push(op, expr);
                    continue;
                }
                case TokenType.CARET: {
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseParentheticalOrUnaryPrefix();
                    stack.push(op, expr);
                    continue;
                }
            }
            break;
        }

        if (stack.length % 2 !== 1) {
            throw "stack should be odd-sized";
        }

        if (stack.length === 1) {
            return stack[0];
        }
        else {
            let result = BinaryOperator(stack[0], stack[1] as Terminal, stack[2]);
            for (let i = 3; i < stack.length; i += 2) {
                result = BinaryOperator(result, stack[i] as Terminal, stack[i+1]);
                i += 2;
            }
            return result;
        }
    }

    function parseParentheticalOrUnaryPrefix(allowUnary = true) : Node {
        switch (lookahead()) {
            case TokenType.LEFT_PAREN: {
                const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                const expr = parseAnonymousFunctionDefinitionOrExpression();
                const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                let root : Node = Parenthetical(leftParen, expr, rightParen);

                //
                // this message could be better especially at a use site like a concise arrow function:
                // `() => ({x:1})` is illegal, but the message could be "Consider an explicit return"
                // anyway, at least its flagged
                //
                switch (stripOuterParens(root).type) {
                    case NodeType.arrayLiteral:
                        parseErrorAtRange(root.range, "Parenthesized array literals are illegal. Consider removing the parentheses.");
                        break;
                    case NodeType.structLiteral:
                        parseErrorAtRange(root.range, "Parenthesized struct literals are illegal. Consider removing the parentheses.");
                        break;
                }

                root = parseTrailingExpressionChain(root);

                // (function() {})()      valid cf2021+
                // (() => value)()        valid cf2021+
                // (function() {}).v()    invalid
                // ([1,2,3])[1]           invalid
                // ({x:1}).x              invalid
                // ({x:1})["x"]           invalid
                //
                if ((root.type === NodeType.indexedAccess) ||
                    (root.type === NodeType.unaryOperator && root.expr.type === NodeType.indexedAccess) ||
                    (root.type === NodeType.callExpression && root.left.type === NodeType.indexedAccess)) {
                    parseErrorAtRange(root.range, "Illegal indexed access expression; consider removing the parentheses from the left-most expression");
                }
                return root;
            }
            case TokenType.LIT_NOT: // [[fallthrough]];
            case TokenType.EXCLAMATION: {
                if (allowUnary) {
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseParentheticalOrUnaryPrefix(); // for unary NOT operator, recurse to support !!x and similar
                    return UnaryOperator(op, expr);
                }
                // else fallthrough
            }
            case TokenType.MINUS:
            case TokenType.PLUS:
            case TokenType.DBL_MINUS: // [[fallthrough]];
            case TokenType.DBL_PLUS: {
                if (allowUnary) {
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseParentheticalOrUnaryPrefix(/*allowUnary*/ false);
                    return UnaryOperator(op, expr);
                }
                // else fallthrough
            }
            default: {
                return parseCallExpressionOrLower();
            }
        }
    }

    function parseHashWrappedExpression() {
        if (isInSomeContext(ParseContext.hashWrappedExpr)) throw "parseHashWrappedExpr cannot be nested";

        const savedContext = parseContext;
        parseContext |= ParseContext.hashWrappedExpr;

        const leftHash = parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);

        let expr : Node;
        if (isInSomeContext(ParseContext.interpolatedText)) {
            // if we're in interpolated text, we can do any expression here, like "#x+1#"
            expr = parseExpression();
        }
        else {
            // #foo()#     is ok
            // #foo() + 1# is invalid
            expr = parseCallExpressionOrLower()
        }

        const rightHash = parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia, "Unterminated hash-wrapped expression");

        parseContext = savedContext;
        return HashWrappedExpr(leftHash, expr, rightHash);
    }

    function parseCallExpressionOrLower() : Node {
        switch(lookahead()) {
            case TokenType.MINUS:
            case TokenType.NUMBER:
                return parseNumericLiteral();
            case TokenType.QUOTE_DOUBLE: // [[fallthrough]];
            case TokenType.QUOTE_SINGLE:
                return parseStringLiteral();
            case TokenType.KW_TRUE:
            case TokenType.KW_FALSE:
                return BooleanLiteral(parseExpectedTerminal(lookahead(), ParseOptions.withTrivia));
            case TokenType.LEFT_BRACE:
                return parseStructLiteral();
            case TokenType.LEFT_BRACKET:
                return parseArrayLiteralOrOrderedStructLiteral();
            case TokenType.HASH:
                if (!isInSomeContext(ParseContext.hashWrappedExpr)) {
                    return parseHashWrappedExpression();
                }
                // [[fallthrough]];
            default: break;
        }

        let root : Node = parseIdentifier();
        root = parseTrailingExpressionChain(root);
        return root;
    }

    /**
     * given some root, parse a chain of dot/bracket | call expression accesses, and a postfix ++/-- operator
     * something like `a.b["c"]().d["e"]++`
     */
    function parseTrailingExpressionChain<T extends Node>(root: T) : T | IndexedAccess | CallExpression | UnaryOperator {
        while (true) {
            switch(lookahead()) {
                case TokenType.LEFT_BRACKET:
                case TokenType.DOT:
                    (root as Node) = parseIndexedAccess(root);
                    continue;
                case TokenType.LEFT_PAREN:
                    (root as Node) = parseCallExpression(root);
                    continue;
            }
            break;
        }

        switch (lookahead()) {
            case TokenType.DBL_PLUS:
            case TokenType.DBL_MINUS:
                const unaryOp = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                (root as Node) = UnaryOperator(root, unaryOp);
        }

        return root;
    }

    function parseIndexedAccess<T extends Node>(root: T) : T | IndexedAccess {
        const base = root;
        while (true) {
            switch (lookahead()) {
                case TokenType.LEFT_BRACKET: {
                    const leftBracket = parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                    const expr = isStartOfExpression()
                        ? parseExpression()
                        : (parseErrorAtCurrentToken("Expression expected."), createMissingNode(Identifier(NilTerminal, "")));
                    
                    const rightBracket = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);

                    if (root.type !== NodeType.indexedAccess) {
                        (root as Node) = IndexedAccess(root);
                    }
                    pushAccessElement(root as IndexedAccess, leftBracket, expr, rightBracket);

                    continue;
                }
                case TokenType.DOT: {
                    const dot = parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
                    const propertyName = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ true);
                    if (root.type !== NodeType.indexedAccess) {
                        (root as Node) = IndexedAccess(root);
                    }
                    pushAccessElement(root as IndexedAccess, dot, propertyName);
                    continue;
                }
            }
            break;
        }

        if (root.type === NodeType.indexedAccess) {
            switch (base.type) {
                case NodeType.arrayLiteral:
                    parseErrorAtRange(base.range, "An array literal may not be accessed in the same expression as its definition.");
                    break;
                case NodeType.structLiteral:
                    parseErrorAtRange(base.range, "A struct literal may not be accessed in the same expression as its definition.");
                    break;
            }
        }

        return root;
    }

    function isStartOfExpression() : boolean {
        switch (lookahead()) {
            case TokenType.EOF:
                return false;
            case TokenType.LEFT_PAREN:
            case TokenType.LEFT_BRACE:
            case TokenType.NUMBER:
            case TokenType.DBL_MINUS:
            case TokenType.DBL_PLUS:
            case TokenType.EXCLAMATION:
            case TokenType.LIT_NOT:
            case TokenType.HASH:
            case TokenType.PLUS:
            case TokenType.MINUS: // @fixme: TokenType.NUMBER currently includes the minus, but it should be a prefix unary operator                
            case TokenType.QUOTE_SINGLE:
            case TokenType.QUOTE_DOUBLE:
            case TokenType.KW_TRUE:
            case TokenType.KW_FALSE:
            case TokenType.KW_FUNCTION:
                return true;
            default:
                return isIdentifier();
        }
    }

    function parseCallExpression(root: Node) : CallExpression {
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const args : CallArgument[] = [];
        while (isStartOfExpression()) {
            const exprOrArgName = parseExpression();

            // if we got an identifier, peek ahead to see if there is an equals token
            // if so, this is a named argument, like foo(bar=baz, qux=42)
            // we don't know from our current position if all of the args are named,
            // we can check that later
            if (exprOrArgName.type === NodeType.identifier) {
                if (lookahead() === TokenType.EQUAL) {
                    const equals = parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
                    const namedArgValue = parseExpression();
                    const comma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
                    if (!comma && isStartOfExpression()) {
                        parseErrorAtRange(namedArgValue.range.toExclusive, namedArgValue.range.toExclusive + 1, "Expected ','");
                    }
                    args.push(CallArgument(exprOrArgName, equals, namedArgValue, comma));
                    continue;
                }
            }

            // we have an unnamed arguments, like foo(x, y, z);
            const comma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

            if (!comma && isStartOfExpression()) {
                parseErrorAtRange(exprOrArgName.range.toExclusive, exprOrArgName.range.toExclusive + 1, "Expected ','");
            }

            args.push(CallArgument(null, null, exprOrArgName, comma));
        }
        const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        return CallExpression(root, leftParen, args, rightParen);
    }

    //
    // @todo - the rules around what is and isn't a valid struct key need to be made more clear
    //
    function parseStructLiteralInitializerKey() : Node {
        let result = parseExpression();
        switch (result.type) {
            case NodeType.hashWrappedExpr:
            case NodeType.simpleStringLiteral:
            case NodeType.interpolatedStringLiteral:
                return result;
            case NodeType.identifier: {
                result = DottedPath(result);
                while (lookahead() === TokenType.DOT) {
                    const dot = parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
                    const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ true);
                    result.rest.push({dot:dot, key: name});
                }
                return result;
            }
            case NodeType.indexedAccess:
                if (result.accessElements.every(v => v.accessType === IndexedAccessType.dot)) {
                    return result;
                }
                // else fallthrough
            default: {
                parseErrorAtRange(result.range, "Invalid struct initializer key");
                return result;
            }
        }
    }

    function parseStructLiteral() : Node {
        const leftBrace = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
        const kvPairs : StructLiteralInitializerMember[] = [];

        while (lookahead() !== TokenType.RIGHT_BRACE && lookahead() !== TokenType.EOF) {
            const key = parseStructLiteralInitializerKey();
            const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
            const value = parseExpression();
            const maybeComma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

            if (!maybeComma && lookahead() !== TokenType.RIGHT_BRACE) {
                parseErrorAtRange(value.range.toExclusive - 1, value.range.toExclusive + 1, "Expected ','");
            }
            if (maybeComma && lookahead() === TokenType.RIGHT_BRACE) {
                parseErrorAtRange(maybeComma.range, "Illegal trailing comma");
            }
            kvPairs.push(StructLiteralInitializerMember(key, colon, value, maybeComma));
        }

        const rightBrace = parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
        return parseTrailingExpressionChain(
            StructLiteral(leftBrace, kvPairs, rightBrace));
    }

    /**
     * supports 3 productions:
     * [a,b,c]      --> array literal
     * [a: 1, b: 2] --> ordered struct literal
     * [:]          --> emptry ordered struct literal
     * @returns 
     */
    function parseArrayLiteralOrOrderedStructLiteral() : Node {
        const leftBracket = parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);

        if (lookahead() === TokenType.COLON) {
            const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
            const rightBracket = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);

            return parseTrailingExpressionChain(
                EmptyOrderedStructLiteral(leftBracket, colon, rightBracket));
        }        

        function isExpressionThenColon() {
            parseExpression();
            return lookahead() === TokenType.COLON;
        }

        if (SpeculationHelper.lookahead(isExpressionThenColon)) {
            const structMembers : StructLiteralInitializerMember[] = [];
            while (isStartOfExpression()) {
                const key = parseStructLiteralInitializerKey();
                const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                const expr = parseExpression();
                const maybeComma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

                if (!maybeComma && lookahead() !== TokenType.RIGHT_BRACKET) {
                    parseErrorAtRange(expr.range.toExclusive-1, expr.range.toExclusive, "Expected ','");
                }
                if (maybeComma && lookahead() === TokenType.RIGHT_BRACKET) {
                    parseErrorAtRange(maybeComma.range, "Illegal trailing comma");
                }
                structMembers.push(StructLiteralInitializerMember(key, colon, expr, maybeComma))
            }

            const rightBracket = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);
            return parseTrailingExpressionChain(
                OrderedStructLiteral(leftBracket, structMembers, rightBracket));
        }
        else {
            const elements : ArrayLiteralInitializerMember[] = [];
            while (isStartOfExpression()) {
                const expr = parseExpression();
                const maybeComma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

                if (!maybeComma && lookahead() !== TokenType.RIGHT_BRACKET) {
                    parseErrorAtRange(expr.range.toExclusive-1, expr.range.toExclusive, "Expected ','");
                }
                if (maybeComma && lookahead() === TokenType.RIGHT_BRACKET) {
                    parseErrorAtRange(maybeComma.range, "Illegal trailing comma");
                }

                elements.push(ArrayLiteralInitializerMember(expr, maybeComma))
            }
            const rightBracket = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);
            return parseTrailingExpressionChain(
                ArrayLiteral(leftBracket, elements, rightBracket));
        }
    }

    function isIllegalKeywordTokenAsIdentifier(tokenType: TokenType) {
        return tokenType > TokenType._FIRST_KW
            && tokenType < TokenType._LAST_KW
            && tokenType !== TokenType.KW_VAR; // `var` is a valid identifier, so `var var = expr` is OK
    }

    function isIdentifier() : boolean {
        if (identifierPattern.test(peek().text)) {
            let tokenType : TokenType | undefined = lookahead() === TokenType.LEXEME
                ? TokenTypeUiStringReverse[peek().text.toLowerCase()]
                : lookahead();
            // in tag mode we might get keywords as lexemes
            // so we try to reverse map from text to a token
            // if there was no matching token then it's an acceptable identifier
            // if it did match a token, make sure it isn't an illegal keyword like 'var'
            return !tokenType || !isIllegalKeywordTokenAsIdentifier(tokenType);
        }
        return false;
    }

    function parseIdentifier() : Identifier {
        let gotValidIdentifier = true;
        if (isIllegalKeywordTokenAsIdentifier(lookahead())) {
            parseErrorAtCurrentToken(`Reserved keyword \`${peek().text.toLowerCase()}\` cannot be used as a variable name.`);
            gotValidIdentifier = false;
        }
        else if (!isIdentifier()) {
            if (globalDiagnosticEmitter) globalDiagnosticEmitter();
            else parseErrorAtCurrentToken("Expected an identifier.");
            gotValidIdentifier = false;
        }

        // don't consume if we didn't match a valid identifier
        const terminal = gotValidIdentifier
            ? Terminal(next(), parseTrivia())
            : Terminal(peek(), []);

        const canonicalName = gotValidIdentifier
            ? terminal.token.text.toLowerCase()
            : "";

        return Identifier(terminal, canonicalName);
    }

    function parseStringLiteral() : Node {
        const quoteType = lookahead();
        if (quoteType !== TokenType.QUOTE_SINGLE && quoteType !== TokenType.QUOTE_DOUBLE) {
            // will a lookahead or speculate ever trigger this ... ?
            throw "AssertionFailure: parseStringLiteral called on input without valid string delimiter";
        }

        const leftQuote = parseExpectedTerminal(quoteType, ParseOptions.noTrivia);
        const stringElements = parseInterpolatedText(quoteType);
        const rightQuote = parseExpectedTerminal(quoteType, ParseOptions.withTrivia);

        if (stringElements.length === 1) {
            const onlyElement = stringElements[0];
            if (onlyElement.type === NodeType.textSpan) {
                return SimpleStringLiteral(leftQuote, onlyElement, rightQuote);
            }
        }

        return InterpolatedStringLiteral(quoteType, leftQuote, stringElements, rightQuote);
    }

    function parseNumericLiteral() {
        return NumericLiteral(parseExpectedTerminal(TokenType.NUMBER, ParseOptions.withTrivia));
    }  

    function isJavaLikeTypename() {
        if (!isLexemeLikeToken(peek())) {
            return false;
        }

        next();

        while (lookahead() === TokenType.DOT) {
            next();
            if (isLexemeLikeToken(peek())) next();
            else return false;    
        }

        // @fixme - handle the case of a trailing dot here ?
        // `x.y.z.`
        //       ^ this is probably OK in this predicate; parser has to handle it though

        return true;
    }

    function isJavaLikeTypenameThenName() : boolean {
        if (!isJavaLikeTypename()) return false;
        parseTrivia();
        return isLexemeLikeToken(peek());
    }

    function isJavaLikeTypenameThenFunction() : boolean {
        if (!isJavaLikeTypename()) return false;
        parseTrivia();
        return lookahead() === TokenType.KW_FUNCTION;
    }

    function isIdentifierThenFatArrow() : boolean {
        if (!isIdentifier()) return false;
        next();
        parseTrivia();
        if (lookahead() !== TokenType.EQUAL_RIGHT_ANGLE) return false;
        return true;
    }

    function isAccessModifier() : boolean {
        const peekedText = peek().text;
        const accessModifiers = ["public", "private", "package", "remote"];
        for (const accessModifier of accessModifiers) {
            if (peekedText === accessModifier) return true;
        }
        return false;
    }

    function parseDottedPathTypename() : DottedPath<Terminal> {
        const result = DottedPath<Terminal>(parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia));
        while (lookahead() === TokenType.DOT) {
            result.rest.push({
                dot: parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia),
                key: parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia)
            })
        }
        return result;
    }

    // speculative overload is last in overload set so speculation-forwarder can see it
    // this might not be maintainable in the long run ? the alternative is to not overload, and
    // use definite! syntax at non-speculative call-
    // the speculative mode is to support disambiguating arrow-function calls, which often look like other valid expressions
    // until we get a full parameter definition parse followed by `) <trivia>? =>`
    function tryParseFunctionDefinitionParameters(speculative: false) : FunctionParameter[];
    function tryParseFunctionDefinitionParameters(speculative: true) : FunctionParameter[] | null;
    function tryParseFunctionDefinitionParameters(speculative: boolean) : FunctionParameter[] | null {
        const result : FunctionParameter[] = [];
        // might have to handle 'required' specially in "isIdentifier"
        // we could set a flag saying we're in a functionParameterList to help out
        while (isStartOfExpression()) {
            let requiredTerminal : Terminal | null = null;
            let javaLikeTypename : DottedPath<Terminal> | null = null;
            let name : Identifier;
            let equal : Terminal | null = null;
            let defaultValue : Node | null = null;
            let comma : Terminal | null = null;

            //
            // required is not a keyword, it just has a special use in exactly this position
            //
            // function foo(required (type(.name)*)? name (= defaultExpr)?) { ... }
            //              ^^^^^^^^
            if (peek().text.toLowerCase() === "required") {
                requiredTerminal = parseOptionalTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
            }

            // function foo((required)? type(.name)* name (= defaultExpr)?) { ... }
            //                          ^^^^^^^^^^^^^
            if (SpeculationHelper.lookahead(isJavaLikeTypenameThenName)) {
                javaLikeTypename = parseDottedPathTypename();
                name = parseIdentifier();
            }
            // function foo((required)? name (= defaultExpr)?) { ... }
            //                          ^^^^
            else if (isLexemeLikeToken(peek())) {
                name = parseIdentifier();
            }
            // didn't match anything
            else {
                if (speculative) {
                    return null;
                }
                else {
                    next();
                    parseTrivia();
                    name = createMissingNode(Identifier(NilTerminal, ""));
                }
            }
            
            if (lookahead() === TokenType.LEXEME) {
                const discardedLexemesStartPos = scanner.getIndex();
                do {
                    next();
                } while (lookahead() === TokenType.LEXEME);
                
                // @fixme: should be a warning
                parseErrorAtRange(discardedLexemesStartPos, scanner.getIndex(), "Names in this position will be discarded at runtime; are you missing a comma?")
            }

            equal = parseOptionalTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
            if (equal) {
                defaultValue = parseExpression();
            }

            comma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            result.push(FunctionParameter(requiredTerminal, javaLikeTypename, name, equal, defaultValue, comma));
        }

        return result;
    }

    function tryParseArrowFunctionDefinition() : ArrowFunctionDefinition | null {
        let leftParen : Terminal | null = null;
        let params : FunctionParameter[];
        let rightParen : Terminal | null = null;

        if (SpeculationHelper.lookahead(isIdentifierThenFatArrow)) {
            params = [
                FunctionParameter(
                    null,
                    null,
                    parseIdentifier(),
                    null,
                    null,
                    null)
            ];
        }
        else if (lookahead() === TokenType.LEFT_PAREN) {
            leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
            const maybeParams = SpeculationHelper.speculate(tryParseFunctionDefinitionParameters, /*speculative*/ true);
            if (!maybeParams) {
                return null;
            }
            params = maybeParams;
            rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        }
        else {
            return null;
        }

        //
        // we're in a position where the caller determined the production can descend into an arrow function
        // we got a valid parameters list; but a fat arrow here is what completes the deal
        // e.g,
        // <cfset x = (y)>
        // is a valid assignment of `(y)` to `x`; but it also looks like the start of an arrow function
        //

        const fatArrow = parseOptionalTerminal(TokenType.EQUAL_RIGHT_ANGLE, ParseOptions.withTrivia);
        if (!fatArrow) {
            return null;
        }

        //
        // we enter script mode IF we are entering a braced-block;
        // otherwise, we don't change modes;
        // <cfset identity_lambda = x => <!--- still in tag mode ---> x>
        // <cfset identity_lambda = x => { /* entered script mode */ return x; }>
        //
        const savedMode = mode;
        if (lookahead() === TokenType.LEFT_BRACE) {
            setScannerMode(ScannerMode.script);
        }
        const stmtOrBlock = parseStatement();
        setScannerMode(savedMode);

        return ArrowFunctionDefinition(leftParen, params, rightParen, fatArrow, stmtOrBlock);
    }

    function stripOuterParens(node: Node) {
        while (node.type === NodeType.parenthetical) {
            node = node.expr;
        }
        return node;
    }

    function parseSwitch() {
        const switchToken = parseExpectedTerminal(TokenType.KW_SWITCH, ParseOptions.withTrivia);
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const expr = parseExpression();
        const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        const leftBrace = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);

        const cases : SwitchCase[] = [];

        const savedContext = parseContext;
        parseContext |= ParseContext.switch;

        while (lookahead() !== TokenType.EOF) {
            let thisCase : SwitchCase;
            if (lookahead() === TokenType.RIGHT_BRACE) {
                break;
            }
            else if (lookahead() === TokenType.KW_CASE) {
                const caseToken = parseExpectedTerminal(TokenType.KW_CASE, ParseOptions.withTrivia);
                const caseExpr = parseExpression();
                const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                const body = parseStatementList();
                thisCase = SwitchCase.Case(caseToken, caseExpr, colon, body);
            }
            else if (lookahead() === TokenType.KW_DEFAULT) {
                const defaultToken = parseExpectedTerminal(TokenType.KW_DEFAULT, ParseOptions.withTrivia);
                const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                const body = parseStatementList();
                thisCase = SwitchCase.Default(defaultToken, colon, body);
            }
            else {
                parseErrorAtCurrentToken("Expected a switch case or default here");
                break;
            }

            cases.push(thisCase);
        }

        parseContext = savedContext;
        const rightBrace = parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
        return Switch(switchToken, leftParen, expr, rightParen, leftBrace, cases, rightBrace);
    }

    function parseIf() : Conditional {
        const ifToken = parseExpectedTerminal(TokenType.KW_IF, ParseOptions.withTrivia);
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const expr = parseExpression();
        const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        const stmt = parseStatement();

        const root = Conditional.If(ifToken, leftParen, expr, rightParen, stmt);
        let workingRoot = root;

        while (lookahead() === TokenType.KW_ELSE) {
            const elseToken = parseExpectedTerminal(TokenType.KW_ELSE, ParseOptions.withTrivia);
            const maybeIfToken = parseOptionalTerminal(TokenType.KW_IF, ParseOptions.withTrivia);
            if (maybeIfToken) {
                const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                const expr = parseExpression();
                const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                const stmt = parseStatement();
                workingRoot.alternative = Conditional.ElseIf(elseToken, maybeIfToken, leftParen, expr, rightParen, stmt);
                workingRoot = workingRoot.alternative!;
            }
            else {
                workingRoot.alternative = Conditional.Else(elseToken, parseStatement());
                break;
            }
        }

        return root;
    }

    function parseAnonymousFunctionDefinition() {
        let accessModifier = null;
        let returnType     = null;
        let functionToken  : Terminal;
        let nameToken      = null;
        let leftParen      : Terminal;
        let params         : FunctionParameter[];
        let rightParen     : Terminal;
        let attrs          : TagAttribute[];
        let body           : Block;
        
        functionToken = parseExpectedTerminal(TokenType.KW_FUNCTION, ParseOptions.withTrivia);

        // try to be helpful in this case:
        // <cfset f = function a_name_here_is_an_error() { ... }
        //                     ~~~~~~~~~~~~~~~~~~~~~~~
        if (isLexemeLikeToken(peek())) {
            parseErrorAtCurrentToken("An anonymous function definition cannot have a name.");
            next();
            parseTrivia();
        }

        leftParen  = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        params     = tryParseFunctionDefinitionParameters(/*speculative*/ false);
        rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        attrs      = parseTagAttributes();
        body       = parseBlock(ScannerMode.script);

        return FunctionDefinition(accessModifier, returnType, functionToken, nameToken, leftParen, params, rightParen, attrs, body);
    }

    function tryParseNamedFunctionDefinition(speculative: false) : Node;
    function tryParseNamedFunctionDefinition(speculative: true) : Node | null;
    function tryParseNamedFunctionDefinition(speculative: boolean) : Node | null {
        let accessModifier: Terminal | null = null;
        let returnType    : DottedPath<Terminal> | null = null;
        let functionToken : Terminal | null;
        let nameToken     : Terminal;
        let leftParen     : Terminal;
        let params        : FunctionParameter[];
        let rightParen    : Terminal;
        let attrs         : TagAttribute[];
        let body          : Block;
        
        if (SpeculationHelper.lookahead(isAccessModifier)) {
            accessModifier = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ false);
        }
        if (SpeculationHelper.lookahead(isJavaLikeTypenameThenFunction)) {
            returnType = parseDottedPathTypename();
        }

        if (speculative) {
            functionToken = parseOptionalTerminal(TokenType.KW_FUNCTION, ParseOptions.withTrivia);
            if (!functionToken) {
                return null;
            }
        }
        else {
            functionToken = parseExpectedTerminal(TokenType.KW_FUNCTION, ParseOptions.withTrivia);
        }

        nameToken     = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ false);
        leftParen     = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        params        = tryParseFunctionDefinitionParameters(/*speculative*/ false);
        rightParen    = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        attrs         = parseTagAttributes();
        body          = parseBlock(ScannerMode.script);

        return FunctionDefinition(accessModifier, returnType, functionToken, nameToken, leftParen, params, rightParen, attrs, body);
    }

    function parseDo() : Do {
        const doToken = parseExpectedTerminal(TokenType.KW_DO, ParseOptions.withTrivia);
        const body = parseStatement();
        const whileToken = parseExpectedTerminal(TokenType.KW_WHILE, ParseOptions.withTrivia);
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const expr = parseExpression();
        const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        return Do(doToken, body, whileToken, leftParen, expr, rightParen);
    }

    function parseWhile() : While {
        const whileToken = parseExpectedTerminal(TokenType.KW_WHILE, ParseOptions.withTrivia);
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const expr = parseExpression();
        const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        const body = parseStatement();
        return While(whileToken, leftParen, expr, rightParen, body);
    }

    function parseFor() : For {
        const forToken = parseExpectedTerminal(TokenType.KW_FOR, ParseOptions.withTrivia);
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        let init = isStartOfExpression() ? parseAssignmentOrLower() : null;
        if (peek().text.toLowerCase() === "in") {
            if (!init) {
                init = createMissingNode(Identifier(NilTerminal, ""));
                parseErrorAtCurrentToken("Expression expected.");
            }
            const inToken = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ false);
            const expr = parseExpression();
            const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
            const body = parseStatement();
            return For.ForIn(forToken, leftParen, init, inToken, expr, rightParen, body);
        }
        else {
            const semi1 = parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);
            const condition = isStartOfExpression() ? parseExpression() : null;
            const semi2 = parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);
            const incrementExpr = isStartOfExpression() ? parseAssignmentOrLower() : null;
            const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
            const body = parseStatement();
            return For.For(forToken, leftParen, init, semi1, condition, semi2, incrementExpr, rightParen, body);
        }
    }

    function parseTry() : Try {
        const tryToken   = parseExpectedTerminal(TokenType.KW_TRY, ParseOptions.withTrivia);
        const leftBrace  = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
        const body       = parseStatementList();
        const rightBrace = parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
        
        const catchBlocks : Catch[] = [];
        while (lookahead() === TokenType.KW_CATCH) {
            const catchToken       = parseExpectedTerminal(TokenType.KW_CATCH, ParseOptions.withTrivia);
            const leftParen        = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
            const exceptionType    = parseDottedPathTypename();
            const exceptionBinding = parseIdentifier();
            const rightParen       = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
            const leftBrace        = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
            const catchBody        = parseStatementList();
            const rightBrace       = (leftBrace.flags & NodeFlags.missing) ? createMissingNode(NilTerminal) : parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
            catchBlocks.push(
                Catch(catchToken, leftParen, exceptionType, exceptionBinding, rightParen, leftBrace, catchBody, rightBrace));
        }

        let finallyBlock : Finally | null = null;
        if (lookahead() === TokenType.KW_FINALLY) {
            const finallyToken = parseExpectedTerminal(TokenType.KW_FINALLY, ParseOptions.withTrivia);
            const leftBrace = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
            const body = parseStatementList();
            const rightBrace = parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
            finallyBlock = Finally(finallyToken, leftBrace, body, rightBrace);
        }

        return Try(tryToken, leftBrace, body, rightBrace, catchBlocks, finallyBlock);
    }

    function isDoneWithStatementList() {
        switch (lookahead()) {
            case TokenType.EOF:
            case TokenType.RIGHT_BRACE: // is this The Right Thing ? we're always done when we see '}' ?
                return true;
            case TokenType.CF_END_TAG_START:
                return isInSomeContext(ParseContext.cfScriptTagBody);
            case TokenType.KW_CASE:
            case TokenType.KW_DEFAULT:
                return isInSomeContext(ParseContext.switch);
            default:
                return false;
        }
    }

    function parseStatementList() : Node[] {
        const result : Node[] = [];

        while (!isDoneWithStatementList()) {
            result.push(parseStatement());
        }

        return result;
    }

    function parseBlock(parseBlockInMode: ScannerMode) {
        const savedMode = mode;
        setScannerMode(parseBlockInMode);
        const leftBrace = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
        const body = parseStatementList();

        // if left brace was missing we don't eat right brace
        const rightBrace = (leftBrace.flags & NodeFlags.missing)
            ? createMissingNode(NilTerminal)
            : parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);

        setScannerMode(savedMode);
        return Block(leftBrace, body, rightBrace);
    }

    function parseStatement() : Node {
        outer:
        while (lookahead() !== TokenType.EOF) {
            switch (lookahead()) {
                case TokenType.WHITESPACE: {
                    // @fixme: attach this to a node somehow to get a beter round-trip
                    // do we ever get here ? ... should be just in error, otherwise we would have appropriately eaten any trivia by now
                    next();
                    continue;
                }
                case TokenType.KW_FINAL:
                case TokenType.KW_VAR: {
                    return parseAssignmentOrLower();
                }
                case TokenType.LEFT_BRACE: {
                    // will we *ever* parse a block in tag mode ... ?
                    // anyway, just forward current mode; which is assumed to be script mode, if we're parsing a statement
                    return parseBlock(mode);
                }
                case TokenType.LEFT_PAREN: {
                    const expr = parseParentheticalOrUnaryPrefix();
                    const semicolon = scriptMode() ? parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia) : null;
                    return Statement(expr, semicolon);
                }
                case TokenType.NUMBER: {
                    const expr = parseExpression();
                    const semicolon = scriptMode() ? parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia) : null;
                    return Statement(expr, semicolon);
                }
                case TokenType.KW_SWITCH: {
                    return parseSwitch();
                }
                case TokenType.KW_BREAK: {
                    const breakToken = parseExpectedTerminal(TokenType.KW_BREAK, ParseOptions.withTrivia);
                    // are we ever in tag mode here ? i don't think so
                    const semi = parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);
                    return BreakStatement(breakToken, semi);
                }
                case TokenType.KW_CONTINUE: {
                    const breakToken = parseExpectedTerminal(TokenType.KW_CONTINUE, ParseOptions.withTrivia);
                    // are we ever in tag mode here ? i don't think so
                    const semi = parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);
                    return ContinueStatement(breakToken, semi);
                }
                case TokenType.KW_IF: {
                    return parseIf();
                }
                case TokenType.KW_FUNCTION: {
                    return tryParseNamedFunctionDefinition(/*speculative*/ false);
                }
                case TokenType.KW_FOR: {
                    return parseFor();
                }
                case TokenType.KW_DO: {
                    return parseDo();
                }
                case TokenType.KW_WHILE: {
                    return parseWhile();
                }
                case TokenType.SEMICOLON: {
                    // lone semicolons are valid, they are simply null statements
                    return Statement(null, parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia));
                }
                case TokenType.KW_TRY: {
                    return parseTry();
                }
                case TokenType.CF_END_TAG_START: {
                    if (isInSomeContext(ParseContext.cfScriptTagBody)) {
                        break outer;
                    }
                    else {
                        parseErrorAtCurrentToken("Unexpected </cf token in a statement context");
                        next();
                        continue;
                    }
                }
                case TokenType.KW_RETURN: {
                    const returnToken = parseExpectedTerminal(TokenType.KW_RETURN, ParseOptions.withTrivia);
                    const expr = isStartOfExpression() ? parseAnonymousFunctionDefinitionOrExpression() : null;
                    // if we've got a return statement we should be in a script context;
                    // but a user may not be heeding that rule
                    const semi = scriptMode() ? parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia) : null;
                    return ReturnStatement(returnToken, expr, semi);
                }
                default: {
                    // @todo - check that something is an identifier;
                    // right now a statment like `x:1` is parsed as ident ident number
                    const peeked = peek();
                    if (isLexemeLikeToken(peeked)) {
                        const maybeFunction = SpeculationHelper.speculate(tryParseNamedFunctionDefinition, /*speculative*/ true);
                        if (maybeFunction) {
                            return maybeFunction;
                        }
                    }

                    if (isNamedBlockName(peeked.text)) {
                        const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ false);
                        const attrs = parseTagAttributes();
                        const block = parseBlock(mode);
                        return NamedBlockFromBlock(name, attrs, block);
                    }
                    else {
                        const index = getIndex();
                        const savedDiagnosticEmitter = globalDiagnosticEmitter;
                        globalDiagnosticEmitter = () => parseErrorAtRange(index, getIndex(), "Expression expected");
                        
                        const result = parseAssignmentOrLower();
                        
                        globalDiagnosticEmitter = savedDiagnosticEmitter;

                        if (getIndex() === index) {
                            //throw "assertion failure - consumed no input"
                            //parseErrorAtCurrentToken("Unexpected input");
                            next();
                        }

                        return result;
                        // we can just skip it, there was an error earlier and this is some token that should be part of another
                        // production but is now loose
                        // should we mark an error?; note that we're not returning, we're t
                        /*parseErrorAtCurrentToken("unrecognized construct here");
                        next();
                        parseTrivia();
                        continue;*/
                    }
                }
            }
        }

        //
        // hit EOF
        // something like `if (x y z <EOF>`, y and z are discarded as not being recognized, and we keep advancing looking
        // for a recognizable statement; then we hit EOF
        // return nil statement
        //
        const nilStatement = Statement(null, null);
        nilStatement.range = scanner.currentToken().range;
        return nilStatement;
    }

    function getDiagnostics() : readonly Diagnostic[] {
        return diagnostics;
    }
}
