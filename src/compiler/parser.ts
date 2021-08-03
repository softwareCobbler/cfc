import {
    Diagnostic,
    setDebug as setNodeFactoryDebug,
    CfTag, Node, NodeType, TagAttribute, NodeFlags, Terminal, Comment, TextSpan, NilTerminal,
    Conditional, FromTag, CommentType,
    HashWrappedExpr, BinaryOperator, Parenthetical, UnaryOperator, BooleanLiteral,
    CallExpression, IndexedAccess, pushAccessElement, CallArgument, Identifier, SimpleStringLiteral, InterpolatedStringLiteral,
    NumericLiteral, DottedPath, ArrowFunctionDefinition, Statement, Block, 
    Do,
    While,
    Ternary,
    For,
    KeyedStructLiteralInitializerMember,
    StructLiteral,
    ArrayLiteralInitializerMember,
    ArrayLiteral,
    EmptyOrderedStructLiteral,
    OrderedStructLiteral,
    ReturnStatement,
    BreakStatement,
    ContinueStatement,
    mergeRanges,
    VariableDeclaration,
    ImportStatement,
    New,
    DotAccess, BracketAccess, OptionalDotAccess, OptionalCall, IndexedAccessChainElement, OptionalBracketAccess, IndexedAccessType,
    ScriptSugaredTagCallBlock, ScriptTagCallBlock,
    ScriptSugaredTagCallStatement, ScriptTagCallStatement, SourceFile, Script, Tag, SpreadStructLiteralInitializerMember, StructLiteralInitializerMember, SimpleArrayLiteralInitializerMember, SpreadArrayLiteralInitializerMember, SliceExpression, SymTabEntry, pushDottedPathElement, TypeShim } from "./node";
import { SourceRange, Token, TokenType, ScannerMode, Scanner, TokenTypeUiString, CfFileType, setScannerDebug } from "./scanner";
import { allowTagBody, isLexemeLikeToken, requiresEndTag, getTriviallyComputableString, isSugaredTagName, Mutable} from "./utils";
import { extractCfFunctionSignature, extractScriptFunctionParam, isUnion } from "./types";
import { _Type, cfArray, cfStruct, cfTuple, cfFunctionSignature, cfTypeConstructorInvocation, cfTypeConstructorParam, cfTypeConstructor, cfTypeId, cfUnion, SyntheticType, cfFunctionSignatureParam } from "./types";

let debugParseModule = false;
let parseTypes = false;

const enum ParseOptions {
    none     = 0,
    noTrivia = 0,
    withTrivia        = 0x00000001,
    allowHashWrapped  = 0x00000002,
};

const enum ParseContext {
    none = 0,
    insideCfTagAngles,  // somewhere inside <cf ... > or </cf ... >
    hashWrappedExpr,    // in #...# in an expression context, like `a + #b#`
    cfScriptTagBody,    // in a <cfscript> block; similar to blockstatements, but </cfscript> can terminate it, and a stray `}` does not
    blockStatements,    // inside a { ... }; a stray `}` will terminate this
    for,                // in a for (...) expression
    interpolatedText,   // in <cfoutput>#...#</cfoutput> or "#...#"
    awaitingVoidSlash,  // <cfset foo = bar /> is just `foo=bar`, with a trailing tag-void-slash, not `foo=bar/` with a missing rhs to the `/` operator
    argOrParamList,     // someCallWithArgs(...)
    switchClause,       // in a switch statement
    trivia,             // comments, whitespace
    structLiteralBody,
    arrayLiteralBody,
    sugaredAbort,       // inside an `abort ...;` statement
    typeTupleOrArrayElement,
    typeParamList,
    typeStruct,
    awaitingRightBracket,
    typeAnnotation,
    END                 // sentinel for looping over ParseContexts
}

function TagContext() {
    const depth = {
        output: 0,
        mail: 0,
        query: 0
    };
    
    function inTextInterpolationContext() {
        return depth.output > 0
            || depth.mail > 0
            || depth.query > 0;
    };

    function update(tag: CfTag) {
        let bumpDir = tag.which === CfTag.Which.start ? 1 : -1;
        switch (tag.canonicalName) {
            case "output":
            case "mail":
            case "query": {
                depth[tag.canonicalName] += bumpDir;
            }
        }
    }

    return {
        inTextInterpolationContext,
        update
    }
}

type TagContext = ReturnType<typeof TagContext>;

interface ScannerState {
    mode : ScannerMode;
    index: number;
    artificialEndLimit: number | undefined;
}

export function Parser() {
    function setSourceFile(sourceFile_: SourceFile) {
        sourceFile = sourceFile_;
        scanner = sourceFile.scanner;
        parseContext = ParseContext.none;
        diagnostics = sourceFile.diagnostics;
        return self_;
    }

    function primeLookahead() {
        lookahead_ = peek().type;
    }

    function setScannerMode(mode_: ScannerMode) {
        mode = mode_;
        primeLookahead();
        return self_;
    }

    function setDebug(isDebug: boolean) {
        setNodeFactoryDebug(isDebug);
        setScannerDebug(isDebug);
        debugParseModule = isDebug;
        return self_;
    }

    function setParseTypes(b: boolean) {
        parseTypes = b;
        return self_;
    }

    let scanner : Scanner;
    let sourceFile: SourceFile;
    let mode: ScannerMode;
    let parseContext : ParseContext;
    let lookahead_ : TokenType;
    let token_ : Token;
    let lastNonTriviaToken : Token;
    let diagnostics : Diagnostic[] = [];

    let lastTypeAnnotation : _Type | null = null;
    
    let parseErrorMsg : string | null = null;

    const SpeculationHelper = (function() {
        //
        // run a boolean returning worker, and always rollback changes to parser state when done
        //
        function lookahead(lookaheadWorker: () => boolean) {
            const saveTokenizerState = getScannerState();
            const diagnosticsLimit = diagnostics.length;
            const savedLastNonTriviaToken = lastNonTriviaToken;
            const savedLastTypeAnnotation = lastTypeAnnotation;

            const result = lookaheadWorker();

            diagnostics.splice(diagnosticsLimit); // drop any diagnostics that were added
            restoreScannerState(saveTokenizerState);
            lastNonTriviaToken = savedLastNonTriviaToken;
            lastTypeAnnotation = savedLastTypeAnnotation;
            return result;
        }
        //
        // if speculationWorker returns a truthy `T`, we return that;
        // otherwise, rollback any changes to parser state made by the speculation worker and return null
        //
        function speculate<
            F extends (...args: any) => any,
            Args extends Parameters<F> = Parameters<F>>(speculationWorker: F, ...args: Args) : ReturnType<F> | null {
            const saveTokenizerState = getScannerState();
            const savedParseErrorMsg = parseErrorMsg;
            const diagnosticsLimit = diagnostics.length;
            const savedLastNonTriviaToken = lastNonTriviaToken;
            const savedLastTypeAnnotation = lastTypeAnnotation;

            const result = speculationWorker(...args as [...Args]);

            if (result) {
                return result;
            }
            else {
                restoreScannerState(saveTokenizerState);
                parseErrorMsg = savedParseErrorMsg;
                diagnostics.splice(diagnosticsLimit); // drop any diagnostics that were added
                lastNonTriviaToken = savedLastNonTriviaToken;
                lastTypeAnnotation = savedLastTypeAnnotation;
                return null;
            }
        }
        return {
            lookahead,
            speculate
        }
    })();

    const self_ = {
        setScannerMode,
        setSourceFile,

        setDebug,
        setParseTypes,

        parseTags,
        parseScript,
        parse,
    };

    return self_;

    /*********************************
    /* impl
    /********************************/
    function peek(jump: number = 0) : Token {
        return scanner.peek(jump, mode);
    }

    function lookahead() : TokenType {
        return lookahead_;
    }

    function pos() {
        return scanner.getIndex();
    }

    /**
     * just move the scanner forward and update lookahead
     * @returns 
     */
    function next() : Token {
        const result = scanner.next(mode);
        token_ = result;
        lookahead_ = peek().type;
        return result;
    }

    /**
     * move the scanner forward, update lookahead, run appropriate state updates
     * fixme: get rid of this, just use next()
     */
    function parseNextToken() : Token {
        const token = next();
        if (!isInSomeContext(ParseContext.trivia)) {
            lastNonTriviaToken = token;
        }
        return token;
    }

    function scanToNextToken(token: TokenType[], endOnOrAfter: "on" | "after" = "on") : void {
        scanner.scanToNext(token, mode);
        if (endOnOrAfter === "after") {
            scanner.next(mode);
        }
        primeLookahead();
    }

    function scanToNextChar(char: string, endOnOrAfter: "on" | "after" = "on") : void {
        scanner.scanToNext(char);
        if (endOnOrAfter === "after") {
            scanner.advance();
        }
        primeLookahead();
    }

    function scanTagName() : Token | null {
        const result = scanner.scanTagName();
        if (result && !isInSomeContext(ParseContext.trivia)) lastNonTriviaToken = result;
        primeLookahead();
        return result;
    }

    function scanTagAttributeName() {
        const result = scanner.scanTagAttributeName();
        if (result && !isInSomeContext(ParseContext.trivia)) lastNonTriviaToken = result;
        primeLookahead();
        return result;
    }

    function scanToNextTagCommentToken() {
        scanner.scanToNextTagCommentToken();
        primeLookahead();
    }

    function scanLexemeLikeStructKey() : Token | null {
        const result = scanner.scanLexemeLikeStructKey();
        if (result && !isInSomeContext(ParseContext.trivia)) lastNonTriviaToken = result;
        primeLookahead();
        return result;
    }

    function isIdentifier() : boolean {
        return scanner.isIdentifier();
    }

    function scanIdentifier() : Token | null {
        const result = scanner.scanIdentifier();
        if (result && !isInSomeContext(ParseContext.trivia)) lastNonTriviaToken = result;
        primeLookahead();
        return result;
    }

    function getIndex() {
        return scanner.getIndex();
    }

    function getScannerState() : ScannerState {
        // can maybe put lastNonTriviaToken in here ? and lookahead_ 
        return {
            index: scanner.getIndex(),
            mode: mode,
            artificialEndLimit: scanner.getArtificalEndLimit()
        }
    }

    /**
     * if `state.artificialEndLimit` is undefined, we clear any artifical end limit
     */
    function restoreScannerState(state: ScannerState) {
        scanner.restoreIndex(state.index);
        mode = state.mode;
        if (state.artificialEndLimit !== undefined) {
            scanner.setArtificialEndLimit(state.artificialEndLimit);
        }
        else {
            scanner.clearArtificalEndLimit();
        }
        primeLookahead();
    }

    function tagMode() : boolean {
        return mode === ScannerMode.tag || mode === ScannerMode.allow_both;
    }
    function scriptMode() : boolean {
        return mode === ScannerMode.script || mode === ScannerMode.allow_both;
    }
    function isInSomeContext(context: ParseContext) : boolean {
        return !!(parseContext & (1 << context));
    }

    // same as do in context except we don't stomp on the original flags, just set the new ones
    function doInExtendedContext<T>(context: ParseContext, f: (() => T)) : T {
        const savedContext = updateParseContext(context);
        const result = f();
        parseContext = savedContext;
        return result;
    }

    function doOutsideOfContext<T>(context: ParseContext, f: (() => T)) : T {
        const savedContext = parseContext;
        parseContext &= ~(1 << context);
        const result = f();
        parseContext = savedContext;
        return result;
    }

    /**
     * updates the context to be extended with a new context, and returns the previous context,
     * so it can be restored
     */
    function updateParseContext(newContext: ParseContext) {
        const savedContext = parseContext;
        parseContext |= (1 << newContext);
        return savedContext;
    }

    function parseErrorAtPos(pos: number, msg: string) {
        parseErrorAtRange(pos, pos+1, msg);
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

        if (debugParseModule) {
            const debugFrom = scanner.getAnnotatedChar(freshDiagnostic.fromInclusive);
            const debugTo = scanner.getAnnotatedChar(freshDiagnostic.toExclusive);
            // bump 0-offsetted info to editor-centric 1-offset
            freshDiagnostic.__debug_from_line = debugFrom.line+1;
            freshDiagnostic.__debug_from_col = debugFrom.col+1;
            freshDiagnostic.__debug_to_line = debugTo.line+1;
            freshDiagnostic.__debug_to_col = debugTo.col+1;
        }

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
        if (token_) parseErrorAtRange(token_.range, msg);
    }

    function createMissingNode<T extends Node>(node: T) {
        node.flags |= NodeFlags.error | NodeFlags.missing;
        return node;
    }

    function parseOptionalTerminal(type: TokenType, parseOptions: ParseOptions) : Terminal | null {
        if (lookahead() === type) {
            const token = next();
            if (!isInSomeContext(ParseContext.trivia)) {
                lastNonTriviaToken = token;
            }
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

    function parseExpectedTerminal(type: TokenType, parseOptions: ParseOptions, errorMsg?: string) : Terminal {
        const maybeTerminal = parseOptionalTerminal(type, parseOptions);
        if (maybeTerminal) {
            return maybeTerminal;
        }
        else {
            const actualError = errorMsg ?? "Expected '" + TokenTypeUiString[type] + "'";
            parseErrorAtPos(lastNonTriviaToken.range.toExclusive, actualError);

            const errorPos = pos();
            const emptyRange = new SourceRange(errorPos, errorPos);
            const phonyToken : Token = Token(type, "", emptyRange);
            return createMissingNode(Terminal(phonyToken));
        }
    }

    function parseExpectedTagName() {
        const tagName = scanTagName();

        if (!tagName) {
            parseErrorAtCurrentToken("Expected a tag name.");
            return createMissingNode(Terminal(parseNextToken()));
        }

        return Terminal(tagName, parseTrivia());
    }

    function parseExpectedLexemeLikeTerminal(consumeOnFailure: boolean, allowNumeric: boolean, errorMsg?: string) : Terminal {
        const labelLike = peek();
        let trivia : Node[] = [];
        if (!isLexemeLikeToken(labelLike, allowNumeric)) {
            if (errorMsg) {
                parseErrorAtRange(labelLike.range, errorMsg);
            }
            else {
                parseErrorAtRange(labelLike.range, "Expected a lexeme-like token here.");
            }
            
            if (consumeOnFailure) {
                // consume on failure --- we'll eat trivia if there's trivia where we wanted a lexeme; otherwise, 
                // we don't actually consume anything
                trivia = parseTrivia();
                const forcedLexemeToken = Token(TokenType.LEXEME, "", labelLike.range.fromInclusive, labelLike.range.fromInclusive);
                return Terminal(forcedLexemeToken, trivia);
            }
            else {
                // this "error" lexeme has 0 range
                const forcedLexemeToken = Token(TokenType.LEXEME, labelLike.text, labelLike.range.fromInclusive, labelLike.range.fromInclusive);
                return Terminal(forcedLexemeToken, []);
            }
        }
        else {
            next();
            trivia = parseTrivia();
        }

        if (!isInSomeContext(ParseContext.trivia)) {
            lastNonTriviaToken = labelLike;
        }

        const forcedLexemeToken = Token(TokenType.LEXEME, labelLike.text, labelLike.range.fromInclusive, labelLike.range.toExclusive);
        return Terminal(forcedLexemeToken, trivia);
    }

    function parseTagComment() : CfTag.Comment {
        const commentStart = parseExpectedTerminal(TokenType.CF_TAG_COMMENT_START, ParseOptions.noTrivia);
        const nestedComments : CfTag.Comment[] = [];
        while (true) {
            scanToNextTagCommentToken();
            if (lookahead() === TokenType.CF_TAG_COMMENT_START) {
                nestedComments.push(parseTagComment());
                continue;
            }
            else {
                break;
            }
        }
        
        let result : CfTag.Comment;
        if (lookahead() !== TokenType.CF_TAG_COMMENT_END) {
            if (nestedComments.length > 0) parseErrorAtRange(commentStart.range.fromInclusive, nestedComments[0].range.fromInclusive, "Unterminated tag comment.");
            else parseErrorAtRange(commentStart.range.fromInclusive, scanner.getIndex(), "Unterminated tag comment.");
            const commentEnd = createMissingNode(Terminal(parseNextToken()));
            result = CfTag.Comment(commentStart, nestedComments, commentEnd);
        }
        else {
            const commentEnd = parseExpectedTerminal(TokenType.CF_TAG_COMMENT_END, ParseOptions.noTrivia);
            result = CfTag.Comment(commentStart, nestedComments, commentEnd);
        }

        if (parseTypes) {
            parseTypeAnnotationsFromPreParsedTrivia(result);
        }

        return result;
    }

    function parseScriptSingleLineComment() : Comment {
        const start = parseExpectedTerminal(TokenType.DBL_FORWARD_SLASH, ParseOptions.noTrivia);
        scanToNextChar("\n", /*endOnOrAfter*/"after");
        return Comment(CommentType.scriptSingleLine, new SourceRange(start.range.fromInclusive, scanner.getIndex()));
    }

    function parseScriptMultiLineComment() : Comment {
        const startToken = parseExpectedTerminal(TokenType.FORWARD_SLASH_STAR, ParseOptions.noTrivia);
        scanToNextToken([TokenType.STAR_FORWARD_SLASH]);
        const endToken = parseExpectedTerminal(TokenType.STAR_FORWARD_SLASH, ParseOptions.noTrivia, "Unterminated multiline script comment.");
        return Comment(CommentType.scriptMultiLine, new SourceRange(startToken.range.fromInclusive, endToken.range.toExclusive));
    }

    /**
     * parse triva - comments and whitespace
     * what type of comments are parsed depends on current scanner mode (tag | script)
     * we need to consider the case where we are parsing types, do we parse both tag and script comments?
     */
    function parseTrivia() : Node[] {
        let result : Node[];

        const savedContext = updateParseContext(ParseContext.trivia);

        if (tagMode()) {
            result = [];
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

            //
            // if we're in a tag or a hash-wrapped expr in tag mode, we want to parse tag trivia,
            // but immediately convert it to script-like trivia
            // this way, the tag treeifier doesn't need to concern itself with descending into hash-wrapped exprs to perform these transformations
            // (where it is otherwise responsible for transforming all tags into some common script syntax tree)
            // all other trivia during tag-treeification is loose
            // @fixme: does this only handle "top-level" comments, i.e., we need to handle `#1 + <!--- foo ---> 2#`
            // also the same logic applies to something like <cfset x = {x: "tag comment in struct" <!--- tag comment here --->}
            // which is maybe just on script-like tags
            //
            if (isInSomeContext(ParseContext.hashWrappedExpr) || isInSomeContext(ParseContext.insideCfTagAngles)) {
                result = (result as CfTag[]).map((v) => {
                    if (v.tagType === CfTag.TagType.comment) {
                        return Comment(v);
                    }
                    else {
                        if (debugParseModule) {
                            return TextSpan(v.range, scanner.getTextSlice(v.range));
                        }
                        else {
                            return TextSpan(v.range, "");
                        }
                    }
                })
            }
        }
        else {
            result = [];
            while (true) {
                switch (lookahead()) {
                    case TokenType.DBL_FORWARD_SLASH:
                        result.push(parseScriptSingleLineComment());
                        continue;
                    case TokenType.FORWARD_SLASH_STAR:
                        result.push(parseScriptMultiLineComment());
                        continue;
                    case TokenType.WHITESPACE:
                        if (debugParseModule) {
                            const nextToken = next();
                            result.push(TextSpan(nextToken.range, scanner.getTextSlice(nextToken.range)));
                        }
                        else {
                            result.push(TextSpan(next().range, ""));
                        }
                        continue;
                }
                break;
            }

            // parse types if necessary; in script mode we do this here;
            // in tag mode the tagComment parser handles it
            if (parseTypes) {
                parseTypeAnnotationsFromPreParsedTrivia(result);
            }
        }

        parseContext = savedContext; // clear trivia context; parsing types inside comments is not considered trivia

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
                const expr = doInExtendedContext(ParseContext.awaitingVoidSlash, parseAssignmentOrLower);
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
                    const expr = doInExtendedContext(ParseContext.awaitingVoidSlash, parseAnonymousFunctionDefinitionOrExpression);
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

                    // capture the full range of the script body, which is hard to figure out after the fact,
                    // for example, an unterminated script comment will eat any remaining tags, and just be parsed as trivia,
                    // and so if we use "range" (as we usually do) rather than "range with trivia", we'll get an incorrect result
                    // this has implications in the tag treeifier, where we use the "last tag position" to compute missing tag zero-length positions
                    // if the last tag is a <cfscript> tag and the cfscript ate to EOF, the "last tag position" of the <cfscript>'s right angle is incorrect

                    const startPos = pos();            // start pos is before any possible trivia
                    rightAngle.trivia = parseTrivia(); // <cfscript> is a tag but it gets script-based trivia (so it can have script comments attached to it)
                    const stmtList = parseList(ParseContext.cfScriptTagBody, parseStatement);
                    const endPos = pos();

                    setScannerMode(savedMode);
                    return CfTag.Script(tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, stmtList, new SourceRange(startPos, endPos));
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

        let attrNameLexeme;
        while (attrNameLexeme = scanTagAttributeName()) {
            const attrName = Terminal(attrNameLexeme, parseTrivia());
            if (lookahead() === TokenType.EQUAL) {
                const equal = parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
                let value : Node;

                if (tagMode()) {
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
                        value = createMissingNode(NilTerminal(pos()));
                    }
                }
                else /* scriptMode */ {
                    value = parseExpression();
                }

                result.push(TagAttribute(attrName, attrName.token.text.toLowerCase(), equal, value));
            }
            else {
                result.push(TagAttribute(attrName, attrName.token.text.toLowerCase()));
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
        function isTagComponentOrInterfaceBlock() {
            setScannerMode(ScannerMode.tag);
            if (lookahead() !== TokenType.CF_START_TAG_START) {
                return false;
            }
            const tag = parseCfStartTag();
            return tag.canonicalName === "component" || tag.canonicalName === "interface";
        }

        function isScriptComponentOrInterfaceBlock() {
            setScannerMode(ScannerMode.script);
            const peekedText = peek().text.toLowerCase();
            if (peekedText !== "component" && peekedText !== "interface") {
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
                    if (SpeculationHelper.lookahead(isScriptComponentOrInterfaceBlock)) {
                        match = "script";
                        break outer;
                    }
                    gotNonComment = true;
                    next();
                    continue;
                }
                case TokenType.CF_START_TAG_START: {
                    if (SpeculationHelper.lookahead(isTagComponentOrInterfaceBlock)) {
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

    function parse(cfFileType: CfFileType = sourceFile?.cfFileType || CfFileType.cfm) : Node[] {
        switch (cfFileType) {
            case CfFileType.cfm:
                sourceFile.content = parseTags();
                break;
            case CfFileType.cfc: {
                const componentType = parseComponentPreamble();
                if (!componentType) {
                    sourceFile.content = [];
                    break;
                }
                else if (componentType === "tag") {
                    sourceFile.content = parseTags();
                    break;
                }
                else {
                    sourceFile.content = parseScript();
                    break;
                }
            }
            case CfFileType.dCfm: {
                sourceFile.content = parseTypeAnnotations(/*asDeclarationFile*/ true);
                break;
            }
        }

        return sourceFile.content;
    }

    function parseScript() : Node[] {
        setScannerMode(ScannerMode.script);
        return parseList(ParseContext.blockStatements, parseStatement);
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
        function treeifyTagList(tagList: (CfTag|HashWrappedExpr)[]) : Node[] {
            const openTagStack : string[] = [];
            let tagIndex = 0;

            function openTagStackFindMatchingStartTag(tagCanonicalName: string) : number | null {
                for (let i = openTagStack.length-1; i >= 0; i--) {
                    if (openTagStack[i] === tagCanonicalName) return i;
                }
                return null;
            }

            const enum ReductionScheme {
                // follow the "default" code path, whatever that may be at the use-site that checks for the value
                default,
                // return to parent and perform a reduction
                return,
                // return to parent and do not perform a reduction
                // this does automatically apply to nested blocks;
                // currently we only need it for returning non-statement-wrapped <cfargument> tags in a <cffunction>
                returnRawTags
            }; 
            interface ReductionInstruction {
                onHitWhich: CfTag.Which | Symbol,
                onHitName: string,
                reduction: ReductionScheme;
            }
            const ERROR_REDUCTION = Symbol("onHitWhich:Error");
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

            function stopAt(which: CfTag.Which, name: string, reduction: ReductionScheme) : ReductionInstruction; // overload 1
            function stopAt(which: Symbol,                    reduction: ReductionScheme) : ReductionInstruction; // overload 2
            function stopAt(which: CfTag.Which | Symbol, nameOrReduction: string | ReductionScheme, reduction?: ReductionScheme) : ReductionInstruction {
                return {
                    onHitWhich: which,
                    onHitName: typeof nameOrReduction === "string" ? /*ol1*/ nameOrReduction : /*ol2*/ "",
                    reduction: typeof nameOrReduction === "string" ? /*ol1*/ reduction! : /*ol2*/ nameOrReduction
                };
            }

            function getReductionScheme(hit: CfTag | Symbol, instructions: readonly ReductionInstruction[]) : ReductionScheme {
                function isTag(val: any) : val is CfTag {
                    return typeof val !== "symbol";
                }
                for (const instr of instructions) {
                    if (typeof instr.onHitWhich === "symbol") {
                        if (instr.onHitWhich === hit) {
                            return instr.reduction;
                        }
                    }
                    else {
                        if (isTag(hit) && instr.onHitWhich === hit.which && instr.onHitName === hit.canonicalName) {
                            return instr.reduction;
                        }
                    }
                }

                return ReductionScheme.default;
            }

            function hasNextTag() : boolean {
                return tagIndex < tagList.length;
            }

            // @ts-expect-error - unused
            function next() : never { // shadow the outer next()
                throw "use nextTag";
            }
            // @ts-expect-error - unused
            function peek() : never { // shadow the outer peek()
                throw "use peekTag";
            }
            
            function nextTag() {
                return tagList[tagIndex++];
            }
            function peekTag() {
                return hasNextTag() ? tagList[tagIndex] : null;
            }

            function parseOptionalTag(which: CfTag.Which, canonicalName: string) : CfTag | null {
                if (!hasNextTag()) return null;
                const tag = peekTag()!;
                if (tag.kind === NodeType.hashWrappedExpr) return null;

                if (tag.which === which && tag.canonicalName === canonicalName) {
                    nextTag();
                    return tag;
                }
                else {
                    return null;
                }
            }

            function getTagErrorPos() {
                if (hasNextTag()) {
                    return peekTag()!.range.fromInclusive;
                }
                else {
                    // if there is no next tag, we have already hit the final tag and consumed it;
                    // so return the END of the last tag, because that is where our "cursor" is
                    // unlike the scanner there is no magic EOF marker
                    // generally, tags (and in-tag hash-wrapped exprs) do not consume trivia on their tagEnd terminal, because that is possible HTML content, not trivia

                    const lastTag = tagList[tagList.length-1];

                    // a <cfscript> tag as last tag means the script body consumed to EOF
                    if (lastTag.kind === NodeType.tag && lastTag.tagType === CfTag.TagType.script) {
                        return lastTag.scriptRange.toExclusive;
                    }
                    else {
                        return lastTag.range.toExclusive;
                    }
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
                        const nilTerminal = NilTerminal(getTagErrorPos());
                        missingTag = CfTag.Common(which, nilTerminal, nilTerminal, null, nilTerminal, canonicalName, [])
                    }
                    else {
                        const nilTerminal = NilTerminal(getTagErrorPos());
                        missingTag = CfTag.Common(which, nilTerminal, nilTerminal, null, nilTerminal, canonicalName)
                    }
                    createMissingNode(missingTag);
                    return missingTag;
                }
            }

            function treeifyTagConditional() {
                const ifTag = parseExpectedTag(CfTag.Which.start, "if") as CfTag.ScriptLike;
                openTagStack.push("if");
                
                //
                // this is a kind-of RL approach, we'll scan and hold onto elseif/else tag/blocks (terminating after we see an else)
                // after we match to the </cfif> we reduce to a single Tag.Conditional node
                //
                const rootConsequent = treeifyTags(...reductionInstructions.cfif);
                const rootConsequentAsBlock = FromTag.looseStatementsBlock(rootConsequent);

                let elseIfs : [CfTag.ScriptLike, Block][] = [];
                let else_ : Conditional | null = null;

                while (true) {
                    const elseIfTag = parseOptionalTag(CfTag.Which.start, "elseif") as CfTag.ScriptLike;
                    if (elseIfTag) {
                        const consequent = treeifyTags(...reductionInstructions.cfelseif);
                        const consequentAsBlock = FromTag.looseStatementsBlock(consequent);
                        elseIfs.push([elseIfTag, consequentAsBlock]);
                        continue;
                    }
                    const elseTag = parseOptionalTag(CfTag.Which.start, "else") as CfTag.Common;
                    if (elseTag) {
                        const consequent = treeifyTags(...reductionInstructions.cfelse);
                        const consequentAsBlock = FromTag.looseStatementsBlock(consequent);
                        else_ = Tag.Else(elseTag, consequentAsBlock);
                    }
                    break;
                }

                const endTag = parseExpectedTag(CfTag.Which.end, "if", () => parseErrorAtRange(ifTag.range, "Missing </cfif> tag.")) as CfTag.Common;

                //
                // reduce from </cfif> back to <cfif>
                //
                let root : Tag.Conditional | null = else_ || null;

                for (let i = elseIfs.length-1; i >= 0; i--) {
                    root = Tag.ElseIf(elseIfs[i][0], elseIfs[i][1], root);
                }

                root = Tag.If(ifTag, rootConsequentAsBlock, root, endTag);
                openTagStack.pop();
                return root;
            }

            function treeifyTagFunction(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) {
                const params : Tag.FunctionParameter[] = [];

                function getTriviaOwner() : Node[] {
                    if (params.length === 0) {
                        return startTag.tagEnd.trivia;
                    }
                    else {
                        return params[params.length-1].tagOrigin.startTag!.tagEnd.trivia;
                    }
                }

                let i = 0;
                while (i < body.length) {
                    const node = body[i];
                    if (node.kind === NodeType.textSpan || node.kind === NodeType.comment) {
                        getTriviaOwner().push(node);
                        i++;
                        continue;
                    }
                    if (node.kind === NodeType.tag && node.canonicalName === "argument") {
                        params.push(Tag.FunctionParameter(node as CfTag.Common));
                        i++;
                        continue;
                    }
                    break;
                }

                body = body.splice(i);
                for (const node of body) {
                    if (node.kind === NodeType.tag && node.which === CfTag.Which.start && node.canonicalName === "argument") {
                        parseErrorAtRange(node.range, "<cfargument> tags should precede any other tags within a <cffunction> body.");
                    }
                }
                const result = Tag.FunctionDefinition(startTag, params, looseTagsToStatements(body), endTag);
                if (startTag.typeAnnotation) result.typeAnnotation = startTag.typeAnnotation;
                return result;
            }

            function treeifyTagTry(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) {
                const mainBody : Node[] = [];
                const catchBlocks : Tag.Catch[] = [];
                let finallyBlock : Tag.Finally | null = null;
                
                let gotCatch = false;
                let gotFinally = false;

                let index = 0;

                while (index < body.length) {
                    const node = body[index];
                    if ((node.kind === NodeType.catch)) {
                        gotCatch = true;
                        break;
                    }
                    else if (node.kind === NodeType.finally) {
                        gotFinally = true;
                        break;
                    }
                    else {
                        mainBody.push(node);
                        index++;
                    }
                }

                function getTriviaOwner() : Node[] {
                    if (finallyBlock) {
                        return finallyBlock.tagOrigin.endTag!.tagEnd.trivia;
                    }
                    else if (gotCatch) {
                        return (catchBlocks[catchBlocks.length-1].tagOrigin.endTag?.tagEnd.trivia // if a <catch>...</catch> block, it has an end tag
                            || catchBlocks[catchBlocks.length-1].tagOrigin.startTag?.tagEnd.trivia)!; // if a <catch /> statement, it is just a start tag
                    }
                    else {
                        if (debugParseModule) throw "no trivia owner?";
                        else return [];
                    }
                }

                if (gotCatch) {
                    while (index < body.length) {
                        const node = body[index];
                        if (node.kind === NodeType.finally) {
                            gotFinally = true;
                            break;
                        }

                        if (node.kind === NodeType.catch) {
                            catchBlocks.push(node as Tag.Catch);
                        }
                        else if (node.kind !== NodeType.textSpan && node.kind !== NodeType.comment) {
                            parseErrorAtRange(node.range, "Only comments and whitespace are valid between <cfcatch> and <cffinally> blocks.")
                            node.flags |= NodeFlags.error; // maybe invalidTagPosition or similar? also maybe mark the whole try block an error?
                            getTriviaOwner().push(node);
                        }
                        
                        index++;
                    }
                }

                if (gotFinally) {
                    finallyBlock = body[index] as Tag.Finally;
                    index++;
                }

                if (index !== body.length) {
                    // after a series of possible <cfcatch> and <cffinally> there is still likely to be at least a text span or comments,
                    // and a user might place tags in this position but they are all invalid
                    // we will attach all of this to the trivia of the end tag of the final catch/finally block
                    //
                    // if there were no catch/finally blocks, we should not get here because all of the <ctry>...</cftry> content would have been consumed
                    // as part of the <cftry> block

                    while (index < body.length) {
                        const node = body[index];
                        if (node.kind !== NodeType.textSpan && node.kind !== NodeType.comment) {
                            parseErrorAtRange(node.range, "Only comments and whitespace are valid between <cfcatch> and <cffinally> blocks.")
                        }
                        getTriviaOwner().push(node);
                        index++;
                    }
                }

                return Tag.Try(startTag, mainBody, catchBlocks, finallyBlock, endTag);
            }

            function treeifyTagSwitch(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) {
                const cases : Tag.SwitchCase[] = [];

                function getTriviaOwner() : Node[] {
                    if (cases.length === 0) {
                        return startTag.tagEnd.trivia;
                    }
                    else {
                        return cases[cases.length-1].tagOrigin.endTag!.tagEnd.trivia;
                    }
                }

                for (let i = 0; i < body.length; i++) {
                    const node = body[i];
                    if (node.kind === NodeType.switchCase) {
                        cases.push(node as Tag.SwitchCase);
                        continue;
                    }
                    else if (node.kind !== NodeType.textSpan && node.kind !== NodeType.comment) {
                        parseErrorAtRange(node.range, "Only comments and whitespace are valid outside of <cfcase> & <cfdefaultcase> blocks inside a <cfswitch> block.");
                    }
                    // push non-case blocks as trivia
                    getTriviaOwner().push(node);
                }

                return Tag.Switch(startTag, cases, endTag);
            }

            function looseTagsToStatements(nodeList: Node[]) {
                for (let i = 0; i < nodeList.length; i++) {
                    const node = nodeList[i];
                    if (node.kind === NodeType.tag) {
                        if (node.tagType !== CfTag.TagType.common) throw "should have been a common tag";
                        nodeList[i] = FromTag.Statement(node);
                    }
                }
                return nodeList;
            }

            function treeifyTags(...reductionInstructions: readonly ReductionInstruction[]) : Node[] {
                const result : Node[] = [];

                function localStackFindMatchingStartTag(tag: CfTag) : number | null {
                    for (let i = result.length - 1; i >= 0; i--) {
                        if (result[i].kind === NodeType.tag) {
                            const stackTag = result[i] as CfTag;
                            if (stackTag.which === CfTag.Which.start && stackTag.canonicalName === tag.canonicalName) {
                                return i;
                            }
                        }
                    }
                    return null;
                }

                // handle a general tag block that requires a matching start/end tag pair
                function tagBlockWorker(tag: CfTag) {
                    openTagStack.push(tag.canonicalName);

                    const startTag = tag;
                    nextTag();
                    const blockChildren = treeifyTags(stopAt(CfTag.Which.end, startTag.canonicalName, ReductionScheme.return));

                    openTagStack.pop();

                    const endTag = parseExpectedTag(CfTag.Which.end, startTag.canonicalName, () => parseErrorAtRange(startTag.range, "Missing </cf" + startTag.canonicalName + "> tag."));
                    result.push(FromTag.Block(startTag, blockChildren, endTag));
                }

                while (hasNextTag()) {
                    const tag = peekTag()!;

                    // we can have tags | HashWrappedExpr in our tag list, since when doing our tag pre-parse, we've scanned
                    // out all the text-spans and interpolated-text contexts (which may contain hash-wrapped expressions)
                    if (tag.kind === NodeType.hashWrappedExpr) {
                        result.push(tag);
                        nextTag();
                        continue;
                    }

                    // text tag placeholders get converted into TextSpans, which is conceptually lifting them out of Tag space
                    if (tag.tagType === CfTag.TagType.text) {
                        result.push(TextSpan(tag.range, scanner.getTextSlice(tag.range)));
                        nextTag();
                        continue;
                    }
                    // comments are lifted out of Tag space; this brings any associated typedefs along with it
                    else if (tag.tagType === CfTag.TagType.comment) {
                        const liftedTagComment = Comment(tag);
                        result.push(liftedTagComment);
                        nextTag();
                        continue;
                    }
                    else if (tag.which === CfTag.Which.start) {
                        const reductionScheme = getReductionScheme(tag, reductionInstructions);
                        switch (reductionScheme) {
                            case ReductionScheme.return:
                                return looseTagsToStatements(result);
                            case ReductionScheme.returnRawTags:
                                return result;
                            case ReductionScheme.default:
                                // ok, no-op: the default reduction scheme for a start tag is to do nothing
                                break;
                        }

                        switch (tag.canonicalName) {
                            case "if": {
                                result.push(treeifyTagConditional());
                                continue;
                            }
                            case "set": {
                                result.push(FromTag.CfSetExpressionWrapper(<CfTag.ScriptLike>tag));
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
                                const body = treeifyTags(
                                    stopAt(CfTag.Which.end, "function", ReductionScheme.returnRawTags),
                                    stopAt(ERROR_REDUCTION, ReductionScheme.returnRawTags));
                                const endTag = parseExpectedTag(CfTag.Which.end, "function", () => parseErrorAtRange(startTag.range, "Missing </cffunction> tag."))
                                openTagStack.pop();
                                result.push(treeifyTagFunction(startTag as CfTag.Common, body, endTag as CfTag.Common));
                                continue;
                            }
                            case "try": {
                                openTagStack.push("try");
                                const startTag = parseExpectedTag(CfTag.Which.start, "try");
                                const body = treeifyTags(stopAt(CfTag.Which.end, "try", ReductionScheme.return));
                                const endTag = parseExpectedTag(CfTag.Which.end, "try", () => parseErrorAtRange(startTag.range, "Missing </cftry> tag."));
                                openTagStack.pop();
                                result.push(treeifyTagTry(startTag as CfTag.Common, body, endTag as CfTag.Common));
                                continue;
                            }
                            case "catch": {
                                if (tag.voidSlash) {
                                    result.push(Tag.Catch(tag as CfTag.Common));
                                    nextTag();
                                    continue;
                                }
                                openTagStack.push("catch");
                                const startTag = parseExpectedTag(CfTag.Which.start, "catch");
                                const body = treeifyTags(stopAt(CfTag.Which.end, "catch", ReductionScheme.return));
                                const endTag = parseExpectedTag(CfTag.Which.end, "catch", () => parseErrorAtRange(startTag.range, "Missing </cfcatch> tag."));
                                openTagStack.pop();
                                result.push(Tag.Catch(startTag as CfTag.Common, body, endTag as CfTag.Common));
                                continue;
                            }
                            case "finally": {
                                openTagStack.push("finally");
                                const startTag = parseExpectedTag(CfTag.Which.start, "finally");
                                const body = treeifyTags(stopAt(CfTag.Which.end, "finally", ReductionScheme.return));
                                const endTag = parseExpectedTag(CfTag.Which.end, "finally", () => parseErrorAtRange(startTag.range, "Missing </cffinally> tag."));
                                openTagStack.pop();
                                result.push(Tag.Finally(startTag as CfTag.Common, body, endTag as CfTag.Common));
                                continue;
                            }
                            case "switch": {
                                openTagStack.push("switch");
                                const startTag = parseExpectedTag(CfTag.Which.start, "switch");
                                const body = treeifyTags(stopAt(CfTag.Which.end, "switch", ReductionScheme.return));
                                const endTag = parseExpectedTag(CfTag.Which.end, "switch", () => parseErrorAtRange(startTag.range, "Missing </cfswitch> tag."));
                                openTagStack.pop();
                                result.push(treeifyTagSwitch(startTag as CfTag.Common, body, endTag as CfTag.Common));
                                continue;
                            }
                            case "case": {
                                openTagStack.push("case");
                                const startTag = parseExpectedTag(CfTag.Which.start, "case");
                                const body = treeifyTags(stopAt(CfTag.Which.end, "case", ReductionScheme.return));
                                const endTag = parseExpectedTag(CfTag.Which.end, "case", () => parseErrorAtRange(startTag.range, "Missing </cfcase> tag."));
                                openTagStack.pop();
                                result.push(Tag.SwitchCase(startTag as CfTag.Common, body, endTag as CfTag.Common));
                                continue;
                            }
                            case "defaultcase": {
                                openTagStack.push("defaultcase");
                                const startTag = parseExpectedTag(CfTag.Which.start, "defaultcase");
                                const body = treeifyTags(stopAt(CfTag.Which.end, "defaultcase", ReductionScheme.return));
                                const endTag = parseExpectedTag(CfTag.Which.end, "defaultcase", () => parseErrorAtRange(startTag.range, "Missing </cfdefaultcase> tag."));
                                openTagStack.pop();
                                result.push(Tag.SwitchDefault(startTag as CfTag.Common, body, endTag as CfTag.Common));
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
                                    // e.g., <cfhttp args (/)?> is essentially a call to a fictitious "cfhttp(args)" function, except it is a statement instead of a value producing expression
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
                            return looseTagsToStatements(result);
                        }
                        else if (reductionScheme === ReductionScheme.returnRawTags) {
                            return result;
                        }

                        // if we can't find the target tag in our local result, this tag has no matching start tag
                        // if this tag isn't allowed to have a block body, it's an error, and gets discarded
                        if (!allowTagBody(tag)) {
                            // fixme, unknown tags should allow tag bodies
                            // parse error - end tag for tag type that does not support tag bodys (like cfargument)
                            nextTag();
                            continue;
                        }

                        const maybeMatchingStartTagIndex = localStackFindMatchingStartTag(tag);
                        if (maybeMatchingStartTagIndex !== null) {
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
                            result.push(FromTag.Block(matchingStartTag, looseTagsToStatements(blockChildren), tag));
                        }
                        else {
                            // this tag might be a mismatched tag,
                            // in which case we return the current results, but do not consume the current tag
                            // this will naturally result in an "unmatched tag" error in the caller
                            const matchingOpenTagIndex = openTagStackFindMatchingStartTag(tag.canonicalName);
                            if (matchingOpenTagIndex !== null) {
                                const reductionScheme = getReductionScheme(ERROR_REDUCTION, reductionInstructions);
                                if (reductionScheme === ReductionScheme.returnRawTags) {
                                    return result;
                                }
                                else {
                                    return looseTagsToStatements(result);
                                }
                            }
                            else {
                                parseErrorAtRange(tag.range, "End tag without a matching start tag (cf" + tag.canonicalName + ").")
                            }
                        }

                        nextTag();
                    }
                    else {
                        throw "non-exhaustive if arms";
                    }
                }

                // if we got here we're out of tags; this is possibly an error, dependening on context
                // caller can indicate that by setting an error reduction scheme
                const reductionScheme = getReductionScheme(ERROR_REDUCTION, reductionInstructions);
                if (reductionScheme === ReductionScheme.returnRawTags) {
                    return result;
                }
                else {
                    return looseTagsToStatements(result);
                }
            }

            return treeifyTags(...reductionInstructions.default);
        }

        const savedMode = mode;
        setScannerMode(ScannerMode.tag);

        const tagContext = TagContext();
        const result : (CfTag | HashWrappedExpr)[] = [];

        while (lookahead() != TokenType.EOF) {
            switch (lookahead()) {
                case TokenType.CF_START_TAG_START: {
                    const savedLastTypeAnnotation = lastTypeAnnotation;
                    const tag = doInExtendedContext(ParseContext.insideCfTagAngles, parseCfStartTag);
                    tagContext.update(tag);
                    if (savedLastTypeAnnotation) {
                        tag.typeAnnotation = savedLastTypeAnnotation;
                    }
                    result.push(tag);
                    lastTypeAnnotation = null;
                    continue;
                }
                case TokenType.CF_END_TAG_START: {
                    const tag = doInExtendedContext(ParseContext.insideCfTagAngles, parseCfEndTag);
                    tagContext.update(tag);
                    result.push(tag);
                    lastTypeAnnotation = null;
                    continue;
                }
                case TokenType.CF_TAG_COMMENT_START: {
                    result.push(parseTagComment());
                    continue;
                }
                default: {
                    if (tagContext.inTextInterpolationContext()) {
                        const savedContext = updateParseContext(ParseContext.interpolatedText);
                        const start = getIndex();

                        while (true) {
                            scanToNextToken([
                                TokenType.HASH,
                                TokenType.CF_START_TAG_START,
                                TokenType.CF_END_TAG_START,
                                TokenType.CF_TAG_COMMENT_START], /*endOnOrAfter*/ "on"); // scan to next will hit the next start of comment or EOF

                            if (lookahead() === TokenType.HASH) {
                                if (peek(1).type === TokenType.HASH) { // a doubled up hash is an escaped hash, just plain text; keep going
                                    next(), next(); // skip past the current and next hash; calling scanToNext(T) when we are looking at a T would not move the scanner
                                    continue;
                                }
                                else {
                                    result.push(CfTag.Text(new SourceRange(start, getIndex())));
                                    result.push(parseHashWrappedExpression());
                                    break;
                                }
                            }
                            else {
                                result.push(CfTag.Text(new SourceRange(start, getIndex())));
                                break;
                            }
                        }

                        parseContext = savedContext;
                    }
                    else {
                        const start = getIndex();
                        scanToNextToken([
                            TokenType.CF_START_TAG_START,
                            TokenType.CF_END_TAG_START,
                            TokenType.CF_TAG_COMMENT_START], /*endOnOrAfter*/ "on"); // scan to next will hit the next start of comment or EOF

                        result.push(CfTag.Text(new SourceRange(start, getIndex())));
                    }
                }
            }
        }

        setScannerMode(savedMode);

        return treeifyTagList(result);
    }  

    function parseStringBody(quoteDelimiter: TokenType.QUOTE_SINGLE | TokenType.QUOTE_DOUBLE, allowInterpolations: boolean) : (TextSpan | HashWrappedExpr)[] {
        // when we enter a new interpolation context, we can reset our hashWrappedExpr flag;
        // hash wrapped expressions don't nest directly, but indirectly is ok; e.g,
        // #someVal & "some-string-literal #nesting_here_is_ok#" & trying_to_nest_here_is_an_error #
        const savedContext = updateParseContext(ParseContext.interpolatedText);
        parseContext &= ~(1 << ParseContext.hashWrappedExpr);

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
        while (!endsParseInContext(ParseContext.none)) {
            switch (lookahead()) {
                case quoteDelimiter: { // doubled-up delimiter; meaning it is an escaped quote
                    if (peek(1).type === quoteDelimiter) {
                        startOrContinueTextRange();
                        next(), next();
                        continue;
                    }
                    else { // single delimiter; we're done, don't eat it because the caller will need it
                        finishTextRange();
                        parseContext = savedContext;
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
                        if (allowInterpolations) {
                            finishTextRange();
                            result.push(parseHashWrappedExpression());
                            continue;
                        }
                        else {
                            parseErrorAtCurrentToken("Unexpected interpolation element.");
                            startOrContinueTextRange();
                            next();
                        }
                    }
                }
                default: {
                    startOrContinueTextRange();
                    scanToNextToken([quoteDelimiter, TokenType.HASH], "on");
                }
            }
        }
        
        finishTextRange();
        parseContext = savedContext;
        return result;
    }

    function isAssignmentTarget(node: Node) : boolean {
        // @fixme: in script mode, it may not be possible to assign to a string
        switch (node.kind) {
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
        const savedLastTypeAnnotation = lastTypeAnnotation;
        function isAssignmentOperator() : boolean {
            switch (lookahead()) {
                case TokenType.EQUAL:
                case TokenType.AMPERSAND_EQUAL:
                case TokenType.PLUS_EQUAL:
                case TokenType.STAR_EQUAL:
                case TokenType.MINUS_EQUAL:
                case TokenType.FORWARD_SLASH_EQUAL:
                case TokenType.PERCENT_EQUAL:
                    return true;
                default:
                    return false;
            }
        }

        const finalModifier = parseOptionalTerminal(TokenType.KW_FINAL, ParseOptions.withTrivia);
        const varModifier = parseOptionalTerminal(TokenType.KW_VAR, ParseOptions.withTrivia);
        const root = parseExpression();

        if (!isAssignmentOperator()) {
            // if we're in a `for` context, we just got the following:
            // for (var x.y.z 
            //      ^^^^^^^^^
            // so we hope to see an `in` following this; otherwise, it *also* needs an initializer
            // but we can flag that in the antecedent `for` parser
            if (!isInSomeContext(ParseContext.for) && varModifier) {
                parseErrorAtRange(root.range, "Variable declarations require initializers.");
            }

            if (root.kind === NodeType.identifier
                || root.kind === NodeType.indexedAccess
                && root.accessElements.every(e => e.accessType === IndexedAccessType.dot || e.accessType === IndexedAccessType.bracket)) {
                // @todo - if the access type is not homogenous cf will error, at least at the top-most scope
                // a["b"]["c"] = 0 declares and inits a = {b: {c: 0}};
                // a["b"].c = 0 is an error ("a" is not defined)
                
                //const identifier = root;//root.kind === NodeType.identifier ? root : Identifier(root, getTriviallyComputableString(root));
                if (isInSomeContext(ParseContext.for)) {
                    return VariableDeclaration(finalModifier, varModifier, root);
                }
            }

            return root;
        }

        if (varModifier) {
            if (lookahead() !== TokenType.EQUAL) {
                parseErrorAtRange(root.range, "A mutating assignment operator cannot be used to initialize a variable.");
            }
        }
        if (!isAssignmentTarget(root)) {
            parseErrorAtRange(root.range, "Left-hand side of assignment is not a valid assignment target.");
        }

        // we definitely have at least one <assignment-target><assignment-operator><expression> 
        let operator = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
        let rhs = parseAnonymousFunctionDefinitionOrExpression();
        let assignmentExpr = BinaryOperator(root, operator, rhs);

        // parse out the rest of a possible chain, e.g,
        // x += x -= x *= x /= x &= x;
        // but note, that these are not value-producing expressions, e.g,
        // `x = (x += x)` is invalid, as is `if (x -= y) {...}`
        while (isAssignmentOperator() && isAssignmentTarget(rhs)) {
            operator = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
            rhs = parseAnonymousFunctionDefinitionOrExpression();
            assignmentExpr = BinaryOperator(root, operator, rhs);
        }

        if (finalModifier || varModifier) {
            // check for missing var modifier in later pass; we might be using final on a valid scope
            // `final local.foo = 42` is OK if we are not in a function
            // `final local['foo'] = 42` is OK
            // `final local[dynamic_key]` is illegal
            // `final user_struct.foo = 42` is always illegal
            // `final no_struct` is illegal

            const declaration = VariableDeclaration(finalModifier, varModifier, assignmentExpr);
            if (savedLastTypeAnnotation) {
                declaration.typeAnnotation = savedLastTypeAnnotation;
                lastTypeAnnotation = null;
            }
            return declaration;
        }
        else {
            if (savedLastTypeAnnotation) {
                assignmentExpr.typeAnnotation = savedLastTypeAnnotation;
                lastTypeAnnotation = null;
            }
            return assignmentExpr;
        }
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
        const savedParseErrorMsg = parseErrorMsg;
        parseErrorMsg = "Expected an expression.";

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
                            parseErrorAtRange(questionMark.range.fromInclusive, colon.range.toExclusive, "Consider treating the null coalescing operator as a single token.");
                        }

                        const syntheticOp = Terminal(Token(TokenType.QUESTION_MARK_COLON, "?:", questionMark.range.fromInclusive, colon.range.toExclusive), colon.trivia);
                        const right = parseAnonymousFunctionDefinitionOrExpression();
                        root = BinaryOperator(root, syntheticOp, right);
                    }
                    else {
                        const ternaryTrue = parseAnonymousFunctionDefinitionOrExpression();
                        const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                        const ternaryFalse = parseAnonymousFunctionDefinitionOrExpression();
                        root = Ternary(root, questionMark, ternaryTrue, colon, ternaryFalse);
                    }
                    continue;
                }
            }
            // if we didn't match any of the above operators, we're done
            break;
        }

        parseErrorMsg = savedParseErrorMsg;
        return root;
    }

    function parseComparisonExpressionOrLower() : Node {
        let root = parseAddition();

        while (true) {
            if (tagMode() && (lookahead() === TokenType.LEFT_ANGLE || lookahead() === TokenType.RIGHT_ANGLE)) {
                break;
            }
            switch (lookahead()) {
                case TokenType.DBL_EQUAL:      		   // [[fallthrough]];
                case TokenType.LIT_EQ:        		   // [[fallthrough]];
                case TokenType.LIT_IS:                 // [[fallthrough]];
                case TokenType.EXCLAMATION_EQUAL:	   // [[fallthrough]];
                case TokenType.LIT_IS_NOT:             // [[fallthrough]];
                case TokenType.LIT_NEQ:           	   // [[fallthrough]];
                case TokenType.TRIPLE_EQUAL:           // [[fallthrough]]; // only valid on cf2021+
                case TokenType.EXCLAMATION_DBL_EQUAL:  // [[fallthrough]]; // only valid on cf2021+
    
                case TokenType.LEFT_ANGLE:    		   // [[fallthrough]]; // invalid in tag mode, but we've already checked for it
                case TokenType.LIT_LT:                 // [[fallthrough]];
                case TokenType.LEFT_ANGLE_EQUAL: 	   // [[fallthrough]];
                case TokenType.LIT_LTE:				   // [[fallthrough]];
                case TokenType.LIT_LE:				   // [[fallthrough]];
    
                case TokenType.RIGHT_ANGLE:    		   // [[fallthrough]]; // invalid in tag mode, but we've already checked for it
                case TokenType.LIT_GT:				   // [[fallthrough]];
                case TokenType.RIGHT_ANGLE_EQUAL: 	   // [[fallthrough]];
                case TokenType.LIT_GTE:				   // [[fallthrough]];
                case TokenType.LIT_GE:                 // [[fallthrough]];
                case TokenType.LIT_CONTAINS:           // [[fallthrough]];
                case TokenType.LIT_DOES_NOT_CONTAIN:   // [[fallthrough]];
                case TokenType.LIT_EQV:                // [[fallthrough]];
                case TokenType.LIT_IMP: {              // [[fallthrough]];
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
                case TokenType.PERCENT:
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
                switch (stripOuterParens(root).kind) {
                    case NodeType.arrayLiteral:
                        parseErrorAtRange(root.range, "Parenthesized array literals are illegal. Consider removing the parentheses.");
                        break;
                    case NodeType.structLiteral:
                        parseErrorAtRange(root.range, "Parenthesized struct literals are illegal. Consider removing the parentheses.");
                        break;
                }

                root = parseCallExpressionOrLowerRest(root);

                // (function() {})()      valid cf2021+
                // (() => value)()        valid cf2021+
                // (function() {}).v()    invalid
                // ([1,2,3])[1]           invalid
                // ({x:1}).x              invalid
                // ({x:1})["x"]           invalid
                //
                if ((root.kind === NodeType.indexedAccess) ||
                    (root.kind === NodeType.unaryOperator && root.expr.kind === NodeType.indexedAccess) ||
                    (root.kind === NodeType.callExpression && root.left.kind === NodeType.indexedAccess)) {
                    parseErrorAtRange(root.range, "Illegal indexed access expression; consider removing the parentheses from the left-most expression.");
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

        const savedContext = updateParseContext(ParseContext.hashWrappedExpr);
        const leftHash = parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);
        const expr = parseExpression();
        const rightHash = parseExpectedTerminal(
            TokenType.HASH,
            isInSomeContext(ParseContext.interpolatedText) ? ParseOptions.noTrivia : ParseOptions.withTrivia, // if inside interpolated text, the 'trivia' outside the right-hash should be interpreted as raw text
            "Unterminated hash-wrapped expression.");

        parseContext = savedContext;
        return HashWrappedExpr(leftHash, expr, rightHash);
    }

    function parseCallExpressionOrLower() : Node {
        switch(lookahead()) {
            case TokenType.DOT:
            case TokenType.NUMBER:
                return parseNumericLiteral();
            case TokenType.QUOTE_DOUBLE: // [[fallthrough]];
            case TokenType.QUOTE_SINGLE:
                return parseStringLiteral();
            case TokenType.KW_TRUE:
                return BooleanLiteral(parseExpectedTerminal(lookahead(), ParseOptions.withTrivia), true);
            case TokenType.KW_FALSE:
                return BooleanLiteral(parseExpectedTerminal(lookahead(), ParseOptions.withTrivia), false);
            case TokenType.LEFT_BRACE:
                return parseCallExpressionOrLowerRest(
                    parseStructLiteral());
            case TokenType.LEFT_BRACKET:
                return parseCallExpressionOrLowerRest(
                    parseArrayLiteralOrOrderedStructLiteral());
            case TokenType.HASH:
                if (!isInSomeContext(ParseContext.hashWrappedExpr)) {
                    // do this outside of an interpolated text context;
                    // inside an interpolated text context, a hash wrapped expression can be any expression
                    // but in a call-expr-or-lower hash wrapped expression, only a non-composite call-expr-or-lower is valid
                    // however, hash-wrapped exprs can appear in expressions inside of interpolated text hash-wrapped expressions, e.g,
                    // <cfoutput>
                    //      #someMethod(#x#)#
                    // </cfoutput>
                    // and we would like `x` in the above to adhere to the call-expr-or-lower requirements
                    return doOutsideOfContext(ParseContext.interpolatedText, parseHashWrappedExpression);
                }
                else {
                    break;
                }
            case TokenType.KW_NEW:
                return parseNewExpression();
            default:
                break;
        }

        let root : Node = parseIdentifier();
        root = parseCallExpressionOrLowerRest(root);
        return root;
    }

    /**
     * given some root, parse a chain of dot/bracket | call expression accesses, and a postfix ++/-- operator
     * something like `a.b["c"]().d["e"]++`
     */
    function parseCallExpressionOrLowerRest<T extends Node>(root: T) : T | IndexedAccess | CallExpression | UnaryOperator {
        (root as Node) = parseCallAndIndexedAccessChain(root);

        switch (lookahead()) {
            case TokenType.DBL_PLUS:
            case TokenType.DBL_MINUS:
                const unaryOp = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                (root as Node) = UnaryOperator(root, unaryOp);
        }

        return root;
    }

    function nextNonTriviaIsDot() : boolean {
        next();
        parseTrivia();
        return lookahead() === TokenType.DOT;
    }

    /**
     * accept a Node, and if it is an IndexedAccess node, push an access element into it
     * if it is not already an IndexedAccess node, convert it to one, and then push the access element into it
     */
    function transformingPushAccessElement(root: Node, accessElement: IndexedAccessChainElement) : IndexedAccess {
        if (root.kind !== NodeType.indexedAccess) {
            root = IndexedAccess(root);
        }
        pushAccessElement(root, accessElement);
        return root;
    }

    function parseCallAndIndexedAccessChain<T extends Node>(root: T) : T | IndexedAccess | CallExpression {
        outer:
        while (true) {
            switch (lookahead()) {
                case TokenType.LEFT_PAREN: {
                    root = parseCallExpression(root) as T;
                    continue;
                }
                case TokenType.QUESTION_MARK: {
                    if (SpeculationHelper.lookahead(nextNonTriviaIsDot)) {
                        const questionMark = parseExpectedTerminal(TokenType.QUESTION_MARK, ParseOptions.withTrivia);
                        const dot          = parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
                        if (questionMark.range.toExclusive !== dot.range.fromInclusive) {
                            parseErrorAtRange(questionMark.range.fromInclusive, dot.range.toExclusive, "Consider treating the optional-access operator as a single token.");
                        }
                        if (lookahead() === TokenType.LEFT_BRACKET) {
                            const leftBracket           = parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                            const expr                  = parseArrayIndexOrSliceExpression();
                            const rightBracket          = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);
                            const optionalBracketAccess = OptionalBracketAccess(questionMark, dot, leftBracket, expr, rightBracket);
                            root                        = transformingPushAccessElement(root, optionalBracketAccess) as T;
                            parseErrorAtRange(questionMark.range.fromInclusive, rightBracket.range.toExclusive, "CF does not support optional bracket access expressions.");
                            continue;
                        }
                        else if (lookahead() === TokenType.LEFT_PAREN) {
                            root = transformingPushAccessElement(root, OptionalCall(questionMark, dot)) as T;
                            root = parseCallExpression(root) as T;
                            parseErrorAtRange(questionMark.range.fromInclusive, (<CallExpression>root).rightParen.range.toExclusive, "CF does not support optional call expressions.");
                            continue;
                        }
                        else {
                            const propertyName = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ true);
                            root = transformingPushAccessElement(root, OptionalDotAccess(questionMark, dot, propertyName)) as T;
                            // todo -- parseError if first char of propertyName is an ascii digit
                            continue;
                        }
                    }
                    else {
                        break outer;
                    }
                }
                case TokenType.LEFT_BRACKET: {
                    const leftBracket = parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                    const expr = parseArrayIndexOrSliceExpression();
                    const rightBracket = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);

                    root = transformingPushAccessElement(root, BracketAccess(leftBracket, expr, rightBracket)) as T;
                    continue;
                }
                case TokenType.DOT: {
                    const dot = parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
                    // allow numeric lexeme-likes, to support:
                    // foo = {4: 42};
                    // bar = foo.4; -- ok, bar == 42;
                    const propertyName = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ true);

                    // `x. y`  is illegal
                    // `x . y` is ok
                    // `x .y`  is ok
                    if (previousElementIsIdentifier() && previousElement()!.range.toExclusive === dot.range.fromInclusive && dot.trivia.length > 0) {
                        parseErrorAtRange(dot.rangeWithTrivia, "Expected a property name.");
                    }

                    root = transformingPushAccessElement(root, DotAccess(dot, propertyName)) as T;
                    continue;
                }
            }
            break;
        }

        return root;

        function parseArrayIndexOrSliceExpression() {
            const first = isStartOfExpression()
                ? parseExpression()
                : lookahead() === TokenType.COLON
                ? parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia)
                : (parseErrorAtCurrentToken("Expression expected."), createMissingNode(Identifier(NilTerminal(pos()), "")));
            
            // we got an expression and the next token is not a colon -- so this is just a basic index expression
            // like `x[e]`
            if (first.kind !== NodeType.terminal && lookahead() !== TokenType.COLON) {
                return first;
            }

            // otherwise, we got a slice expression
            let from : Node | null = first.kind === NodeType.terminal ? null : first;
            let colon1: Terminal = first.kind === NodeType.terminal ? first : parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
            let to : Node | null = isStartOfExpression() ? parseExpression() : null;
            let colon2: Terminal = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
            let stride : Node | null = isStartOfExpression() ? parseExpression() : null;
            return SliceExpression(from, colon1, to, colon2, stride);
        }

        function previousElement() : T | IndexedAccessChainElement {
            if (root.kind !== NodeType.indexedAccess) {
                return root;
            }
            else {
                const element = (root as IndexedAccess).accessElements[(root as IndexedAccess).accessElements.length-1];
                return element;
            }
        }

        function previousElementIsIdentifier() {
            if (root.kind !== NodeType.indexedAccess) {
                return root.kind === NodeType.identifier;
            }

            // if root is an IndexedAccess node, it is because there is at least one contained access element
            const element = (root as IndexedAccess).accessElements[(root as IndexedAccess).accessElements.length-1];
            return element.accessType === IndexedAccessType.dot
        }
    }

    function parseNewExpression() {
        const newToken       = parseExpectedTerminal(TokenType.KW_NEW, ParseOptions.withTrivia);
        const className      = parseDottedPathTypename();
        const callExpression = parseCallExpression(className);
        const newExpression  = New(newToken, callExpression);
        return parseCallExpressionOrLowerRest(newExpression); // is new foo()["some property"] ok ?
    }

    function isStartOfStatement() : boolean {
        switch (lookahead()) {
            case TokenType.KW_BREAK:
            case TokenType.KW_CONTINUE:
            case TokenType.KW_DO:
            case TokenType.KW_FOR:
            case TokenType.KW_IF:
            case TokenType.KW_IMPORT:
            case TokenType.KW_RETURN:
            case TokenType.KW_SWITCH:
            case TokenType.KW_TRY:
            case TokenType.KW_VAR:
            case TokenType.KW_WHILE:
            case TokenType.SEMICOLON:
                return true;
            default:
                return (isSugaredTagName(peek().text.toLowerCase()))
                    || isStartOfExpression();
        }
    }
    isStartOfStatement;

    function isStartOfExpression() : boolean {
        switch (lookahead()) {
            case TokenType.EOF:
                return false;
            case TokenType.HASH:
                return !isInSomeContext(ParseContext.hashWrappedExpr);
            case TokenType.DOT_DOT_DOT:
                return isInSomeContext(ParseContext.arrayLiteralBody)
                    || isInSomeContext(ParseContext.structLiteralBody)
                    || isInSomeContext(ParseContext.argOrParamList);
            case TokenType.LEFT_PAREN:
            case TokenType.LEFT_BRACE:
            case TokenType.LEFT_BRACKET:
            case TokenType.NUMBER:
            case TokenType.DBL_MINUS:
            case TokenType.DBL_PLUS:
            case TokenType.EXCLAMATION:
            case TokenType.LIT_NOT:
            case TokenType.PLUS:
            case TokenType.MINUS:
            case TokenType.QUOTE_SINGLE:
            case TokenType.QUOTE_DOUBLE:
            case TokenType.KW_TRUE:
            case TokenType.KW_FALSE:
            case TokenType.KW_FUNCTION:
            case TokenType.KW_NEW:
                return true;
            default:
                return isIdentifier();
        }
    }

    /**
     * 1. (e)          argument
     * 2. (x=e)        named argument
     * 3. (...e)       spread argument
     * 4. (x=...e)     named spread argument
     */
    function parseArgument() : CallArgument {
        if (lookahead() === TokenType.DOT_DOT_DOT) {
            const dotDotDot = parseExpectedTerminal(TokenType.DOT_DOT_DOT, ParseOptions.withTrivia);
            const expr = parseExpression();
            const comma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            if (!comma && isStartOfExpression()) {
                parseErrorAtRange(expr.range.toExclusive, expr.range.toExclusive + 1, "Expected ','");
            }

            // (#3): spread argument
            return CallArgument(null, null, dotDotDot, expr, comma);
        }

        const exprOrArgName = parseAnonymousFunctionDefinitionOrExpression();

        // if we got an identifier, peek ahead to see if there is an equals or colon token
        // if so, this is a named argument, like foo(bar=baz, qux:42)
        // like within a struct literals, `=` and `:` share the same meaning
        // we don't know from our current position if all of the args are named,
        // we can check that later; if one is, all of them must be
        if (exprOrArgName.kind === NodeType.identifier) {
            let equalOrComma : Terminal | null;
            if (equalOrComma =
                    (parseOptionalTerminal(TokenType.EQUAL, ParseOptions.withTrivia) ??
                    (parseOptionalTerminal(TokenType.COLON, ParseOptions.withTrivia)))) {
                const name = exprOrArgName;
                let dotDotDot : Terminal | null = null;
                let expr : Node;

                if (lookahead() === TokenType.DOT_DOT_DOT) {
                    dotDotDot = parseExpectedTerminal(TokenType.DOT_DOT_DOT, ParseOptions.withTrivia);
                    expr = parseExpression();
                }
                else {
                    expr = parseAnonymousFunctionDefinitionOrExpression();
                }

                const comma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

                if (!comma && isStartOfExpression()) {
                    parseErrorAtRange(expr.range.toExclusive, expr.range.toExclusive + 1, "Expected ','");
                }

                // (#2 / #4): named / named spread
                return CallArgument(name, equalOrComma, dotDotDot, expr, comma);
            }
        }

        // there was no '=' or ':' after the expression,
        // so we have an unnamed arguments, like foo(x, y, z);
        const comma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

        if (!comma && isStartOfExpression()) {
            parseErrorAtRange(exprOrArgName.range.toExclusive, exprOrArgName.range.toExclusive + 1, "Expected ','");
        }

        return CallArgument(null, null, null, exprOrArgName, comma);
    }

    function parseCallExpression(root: Node) : CallExpression {
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const args = parseList(ParseContext.argOrParamList, parseArgument);

        if (args.length > 0) {
            if (args[args.length-1].comma) {
                parseErrorAtRange(args[args.length-1].comma!.range, "Illegal trailing comma.");
            }
        }
        
        const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        return CallExpression(root, leftParen, args, rightParen);
    }

    function startsParseInContext(what: ParseContext) : boolean {
        switch (what) {
            case ParseContext.arrayLiteralBody:
            case ParseContext.structLiteralBody:
            case ParseContext.argOrParamList:
            case ParseContext.typeStruct:
            case ParseContext.typeTupleOrArrayElement:
            case ParseContext.typeParamList:
                return isStartOfExpression();
            case ParseContext.cfScriptTagBody:
            case ParseContext.blockStatements:
            case ParseContext.switchClause:
            case ParseContext.blockStatements:
                return isStartOfStatement();
            default:
                return false; // ? yeah ?
        }
    }

    function endsParseInContext(what: ParseContext) : boolean {
        if (lookahead() === TokenType.EOF) return true;

        switch (what) {
            case ParseContext.argOrParamList:
                return lookahead() === TokenType.RIGHT_PAREN;
            case ParseContext.arrayLiteralBody:
            case ParseContext.typeTupleOrArrayElement:
            case ParseContext.awaitingRightBracket:
                return lookahead() === TokenType.RIGHT_BRACKET;
            case ParseContext.typeParamList:
                return lookahead() === TokenType.RIGHT_ANGLE;
            case ParseContext.structLiteralBody:
            case ParseContext.typeStruct:
            case ParseContext.blockStatements:
                return lookahead() === TokenType.RIGHT_BRACE;
            case ParseContext.switchClause:
                return lookahead() === TokenType.KW_CASE
                    || lookahead() === TokenType.KW_DEFAULT;
            case ParseContext.cfScriptTagBody:
                return lookahead() === TokenType.CF_END_TAG_START;
            case ParseContext.interpolatedText:
            case ParseContext.hashWrappedExpr:
                return lookahead() === TokenType.HASH;
            case ParseContext.insideCfTagAngles:
                return lookahead() === TokenType.RIGHT_ANGLE;
            default:
                return false;
        }
    }

    function canMatchLookaheadWithOuterContext() : boolean {
        for (let someContext = 0; someContext  < ParseContext.END; someContext++) {
            if (parseContext & (1 << someContext )) {
                if (startsParseInContext(someContext) || endsParseInContext(someContext)) {
                    return true;
                }
            }
        }
        return false;
    }

    //
    // @todo - the rules around what is and isn't a valid struct key need to be made more clear
    //
    function parseStructLiteralInitializerKey() : Node {
        const maybeLexemeLikeKey = scanLexemeLikeStructKey();
        if (maybeLexemeLikeKey) {
            return Terminal(maybeLexemeLikeKey, parseTrivia());
        }

        let result = parseExpression();
        switch (result.kind) {
            case NodeType.hashWrappedExpr:
            case NodeType.simpleStringLiteral:
            case NodeType.interpolatedStringLiteral:
                return result;
            default: {
                parseErrorAtRange(result.range, "Invalid struct initializer key.");
                return result;
            }
        }
    }

    function parseStructLiteralInitializerMember(terminator: TokenType.RIGHT_BRACE | TokenType.RIGHT_BRACKET) : StructLiteralInitializerMember {
        // move comma checks into checker, then we can put error range where TS does:
        // {abc: foo def: bar},
        //           ^^^ expected ','
        //
        if (lookahead() === TokenType.DOT_DOT_DOT) {
            const dotdotdot = parseExpectedTerminal(TokenType.DOT_DOT_DOT, ParseOptions.withTrivia);
            const expr = parseExpression();
            const maybeComma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

            if (!maybeComma && lookahead() !== terminator) {
                parseErrorAtRange(expr.range.toExclusive - 1, expr.range.toExclusive + 1, "Expected ','");
            }
            if (maybeComma && lookahead() === terminator) {
                parseErrorAtRange(maybeComma.range, "Illegal trailing comma.");
            }
            return SpreadStructLiteralInitializerMember(dotdotdot, expr, maybeComma);
        }
        else {
            const key = parseStructLiteralInitializerKey();
            const colonOrEqual = lookahead() === TokenType.EQUAL
                ? parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia)
                : parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
            const value = parseAnonymousFunctionDefinitionOrExpression();
            const maybeComma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
    
            if (!maybeComma && lookahead() !== terminator) {
                parseErrorAtRange(value.range.toExclusive - 1, value.range.toExclusive + 1, "Expected ','");
            }
            if (maybeComma && lookahead() === terminator) {
                parseErrorAtRange(maybeComma.range, "Illegal trailing comma.");
            }
            return KeyedStructLiteralInitializerMember(key, colonOrEqual, value, maybeComma);
        }
    }

    function parseStructLiteral() : Node {
        const leftBrace = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
        const kvPairs = parseList(ParseContext.structLiteralBody, parseStructLiteralInitializerMember, TokenType.RIGHT_BRACE);
        const rightBrace = parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
        return parseCallExpressionOrLowerRest(
            StructLiteral(leftBrace, kvPairs, rightBrace));
    }

    /**
     * supports 3 productions:
     * [a,b,c]      --> array literal
     * [a: 1, b: 2] --> ordered struct literal
     * [:]          --> emptry ordered struct literal
     * @returns 
     */
    function parseArrayLiteralOrOrderedStructLiteral() : ArrayLiteral | StructLiteral {
        const leftBracket = parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);

        if (lookahead() === TokenType.COLON) {
            const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
            const rightBracket = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);
            return EmptyOrderedStructLiteral(leftBracket, colon, rightBracket);
        }        

        function isExpressionThenColon() {
            parseExpression();
            return lookahead() === TokenType.COLON;
        }

        if (SpeculationHelper.lookahead(isExpressionThenColon)) {
            const useArrayBodyListTerminator = ParseContext.arrayLiteralBody;
            const structMembers = parseList(useArrayBodyListTerminator, parseStructLiteralInitializerMember, TokenType.RIGHT_BRACKET);
            const rightBracket = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);
            return OrderedStructLiteral(leftBracket, structMembers, rightBracket);
        }
        else {
            const savedContext = updateParseContext(ParseContext.arrayLiteralBody);
            const elements : ArrayLiteralInitializerMember[] = [];
            while (isStartOfExpression()) {
                if (lookahead() === TokenType.DOT_DOT_DOT) {
                    const dotDotDot = parseExpectedTerminal(TokenType.DOT_DOT_DOT, ParseOptions.withTrivia);
                    const expr = parseExpression();
                    const maybeComma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
                    if (!maybeComma && lookahead() !== TokenType.RIGHT_BRACKET) {
                        parseErrorAtRange(expr.range.toExclusive-1, expr.range.toExclusive, "Expected ','");
                    }
                    if (maybeComma && lookahead() === TokenType.RIGHT_BRACKET) {
                        parseErrorAtRange(maybeComma.range, "Illegal trailing comma.");
                    }
                    elements.push(SpreadArrayLiteralInitializerMember(dotDotDot, expr, maybeComma));
                }
                else {
                    const expr = parseAnonymousFunctionDefinitionOrExpression();
                    const maybeComma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

                    if (!maybeComma && lookahead() !== TokenType.RIGHT_BRACKET) {
                        parseErrorAtRange(expr.range.toExclusive-1, expr.range.toExclusive, "Expected ','");
                    }
                    if (maybeComma && lookahead() === TokenType.RIGHT_BRACKET) {
                        parseErrorAtRange(maybeComma.range, "Illegal trailing comma.");
                    }

                    elements.push(SimpleArrayLiteralInitializerMember(expr, maybeComma))
                }
            }
            const rightBracket = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);

            parseContext = savedContext;
            
            return ArrayLiteral(leftBracket, elements, rightBracket);
        }
    }

    function isIllegalKeywordTokenAsIdentifier(tokenType: TokenType) {
        // maybe have some versioning here; 2018+ makes final an illegal identifier? ok in 2016 or below ?
        return tokenType > TokenType._FIRST_KW
            && tokenType < TokenType._LAST_KW
            && tokenType !== TokenType.KW_VAR; // `var` is a valid identifier, so `var var = expr` is OK
    }

    // withTrivia is to support instances where the caller may want to manually consume trivia,
    // in order to grab type annotations that are inside comments
    function parseIdentifier(withTrivia = true) : Identifier {
        let terminal : Terminal;
        let name : string;

        if (!isIdentifier()) {
            parseErrorAtPos(lastNonTriviaToken.range.toExclusive, parseErrorMsg ?? "Expected an identifier.");

            terminal = NilTerminal(pos());
            name = "";
        }
        else {
            if (isIllegalKeywordTokenAsIdentifier(lookahead())) {
                parseErrorAtRange(peek().range, `Reserved keyword \`${peek().text.toLowerCase()}\` cannot be used as an identifier.`);
            }

            terminal = Terminal(scanIdentifier()!, withTrivia ? parseTrivia() : []);
            name = terminal.token.text;
        }

        // @fixme: need to mark node with error flags if there was an error
        return Identifier(terminal, name);
    }

    function parseStringLiteral(allowInterpolations = true) : SimpleStringLiteral | InterpolatedStringLiteral {
        const quoteType = lookahead();
        if (quoteType !== TokenType.QUOTE_SINGLE && quoteType !== TokenType.QUOTE_DOUBLE) {
            // will a lookahead or speculate ever trigger this ... ?
            throw "AssertionFailure: parseStringLiteral called on input without valid string delimiter";
        }

        const leftQuote = parseExpectedTerminal(quoteType, ParseOptions.noTrivia);
        const stringElements = parseStringBody(quoteType, allowInterpolations);
        const rightQuote = parseExpectedTerminal(quoteType, ParseOptions.withTrivia);

        if (stringElements.length === 1) {
            const onlyElement = stringElements[0];
            if (onlyElement.kind === NodeType.textSpan) {
                return SimpleStringLiteral(leftQuote, onlyElement, rightQuote);
            }
        }

        return InterpolatedStringLiteral(quoteType, leftQuote, stringElements, rightQuote);
    }

    function parseNumericLiteral() {
        if (lookahead() === TokenType.DOT) {
            const dot = parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
            const number = parseExpectedTerminal(TokenType.NUMBER, ParseOptions.withTrivia);
            // @fixme: if we're in debug mode, manually creating a Token here will miss out on any __debug info a factory would have otherwise attached to it
            const combinedToken = Token(TokenType.NUMBER, "." + number.token.text, dot.range.fromInclusive, number.range.toExclusive);
            return NumericLiteral(Terminal(combinedToken));
        }
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
        parseIdentifier();
        return lookahead() === TokenType.EQUAL_RIGHT_ANGLE;
    }

    function isAccessModifier() : boolean {
        const peekedText = peek().text.toLowerCase();
        const accessModifiers = ["public", "private", "package", "remote"];
        for (const accessModifier of accessModifiers) {
            if (peekedText === accessModifier) return true;
        }
        return false;
    }

    /**
     * allowTrailingGlob is for import statements like `import foo.bar.*;`
     */
    function parseDottedPathTypename(allowTrailingGlob = false) : DottedPath {
        const result = DottedPath(parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ false));
        while (lookahead() === TokenType.DOT) {
            const dot = parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
            
            if (allowTrailingGlob && lookahead() === TokenType.STAR) {
                const glob = parseNextToken();
                const trivia = parseTrivia();
                const key = Terminal(glob, trivia);
                pushDottedPathElement(result, dot, key);
                break;
            }
            else {
                const key = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ false);
                pushDottedPathElement(result, dot, key);
            }
        }
        return result;
    }

    function parseFunctionDeclarationParameters() : cfFunctionSignatureParam[] {
        const params = tryParseFunctionDefinitionParameters(/*speculative*/ false, /*asDeclaration*/ true);
        return extractScriptFunctionParam(params);
    }

    // speculative overload is last in overload set so speculation-forwarder can see it
    // this might not be maintainable in the long run ? the alternative is to not overload, and
    // use definite! syntax at non-speculative call-
    // the speculative mode is to support disambiguating arrow-function calls, which often look like other valid expressions
    // until we get a full parameter definition parse followed by `) <trivia>? =>`
    function tryParseFunctionDefinitionParameters(speculative: false, asDeclaration: boolean) : Script.FunctionParameter[];
    function tryParseFunctionDefinitionParameters(speculative: true) : Script.FunctionParameter[] | null;
    function tryParseFunctionDefinitionParameters(speculative: boolean, asDeclaration = false) : Script.FunctionParameter[] | null {
        const savedContext = updateParseContext(ParseContext.argOrParamList);
        const result : Script.FunctionParameter[] = [];
        // might have to handle 'required' specially in "isIdentifier"
        // we could set a flag saying we're in a functionParameterList to help out
        while (isStartOfExpression()) {
            let requiredTerminal : Terminal | null = null;
            let javaLikeTypename : DottedPath | null = null;
            let dotDotDot : Terminal | null = null;
            let name : Identifier;
            let equal : Terminal | null = null;
            let defaultValue : Node | null = null;
            let comma : Terminal | null = null;
            let type : _Type | null = null;

            //
            // required is a contextual keyword when in left-most parameter definition position
            //
            // function foo(required (type(.name)*)? name (= defaultExpr)?) {  }
            //              ^^^^^^^^
            if (peek().text.toLowerCase() === "required") {
                requiredTerminal = parseOptionalTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
            }

            //
            // function foo((required)? ...name) {  }
            //
            if (lookahead() === TokenType.DOT_DOT_DOT) {
                dotDotDot = parseExpectedTerminal(TokenType.DOT_DOT_DOT, ParseOptions.withTrivia);
                name = parseIdentifier();
                // todo: parse type annotation
            }
            else {
                // function foo((required)? type(.name)* name (= defaultExpr)?) { ... }
                //                          ^^^^^^^^^^^^^
                if (SpeculationHelper.lookahead(isJavaLikeTypenameThenName)) {
                    javaLikeTypename = parseDottedPathTypename();
                    name = parseIdentifier();
                    // todo - parse type annotation
                }
                // function foo((required)? name (= defaultExpr)?) { ... }
                //                          ^^^^
                else if (isLexemeLikeToken(peek())) {
                    if (asDeclaration) {
                        // we're already in a triva-based type annotation, e.g,
                        // function foo(x /*: (name: type) => void*/)
                        //                   ^^^^^^^^^^^^^^^^^^^^^
                        // in which case we can just parse types without comment-delimiters
                        if (isInSomeContext(ParseContext.typeAnnotation)) {
                            name = parseIdentifier(/*withTrivia*/ true);
                            parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                            type = parseType();
                        }
                        else {
                            name = parseIdentifier(/*withTrivia*/ false);
                            const triviaAndType = parseScriptTriviaWithPossibleTypeAnnotation();
                            if (!triviaAndType.type) {
                                parseErrorAtRange(name.range, "Expected a type annotation.");
                            }
                            (<Terminal>name.source).trivia = triviaAndType.trivia;
                            type = triviaAndType.type;
                        }
                    }
                    else {
                        name = parseIdentifier(/*withTrivia*/ true);
                    }
                }
                else {
                    if (speculative) {
                        return null;
                    }
                    else {
                        next();
                        parseTrivia();
                        name = createMissingNode(Identifier(NilTerminal(pos()), ""));
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
            }

            // function foo((required)? ((... name) | (type(.name)* name)) (= defaultExpr)?) {  }
            //                                                              ^^^^^^^^^^^^^
            equal = parseOptionalTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
            if (equal) {
                defaultValue = parseExpression();
            }

            comma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

            result.push(Script.FunctionParameter(requiredTerminal, javaLikeTypename, dotDotDot, name, equal, defaultValue, comma, type));
        }

        if (result.length > 0 && result[result.length-1].comma) {
            parseErrorAtRange(result[result.length-1].comma!.range, "Illegal trailing comma.");
        }

        parseContext = savedContext;

        return result;
    }

    function tryParseArrowFunctionDefinition() : ArrowFunctionDefinition | null {
        let leftParen : Terminal | null = null;
        let params : Script.FunctionParameter[];
        let rightParen : Terminal | null = null;

        if (SpeculationHelper.lookahead(isIdentifierThenFatArrow)) {
            params = [
                Script.FunctionParameter(
                    null,
                    null,
                    null,
                    parseIdentifier(),
                    null,
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
        if (lookahead() === TokenType.LEFT_BRACE) {
            const block = parseBracedBlock(ScannerMode.script);
            return ArrowFunctionDefinition(leftParen, params, rightParen, fatArrow, block);
        }
        else {
            // having matched a params list with a fat arrow, we're definitely in an arrow function, 
            // but the expression may not have been written yet;
            // speculative or not, we return an arrow function
            if (!isStartOfExpression()) {
                parseErrorAtRange(fatArrow.range.toExclusive, fatArrow.range.toExclusive+1, "Expression expected.");
                return ArrowFunctionDefinition(leftParen, params, rightParen, fatArrow, createMissingNode(NilTerminal(pos())));
            }
            else {
                return ArrowFunctionDefinition(leftParen, params, rightParen, fatArrow, parseAnonymousFunctionDefinitionOrExpression());
            }
        }

    }

    function stripOuterParens(node: Node) {
        while (node.kind === NodeType.parenthetical) {
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

        const cases : Script.SwitchCase[] = [];
        const savedContext = updateParseContext(ParseContext.blockStatements);
        parseContext &= ~(1 << ParseContext.switchClause); // we may be in a nested switch clause, clear the outer flag

        while (!endsParseInContext(ParseContext.blockStatements)) {
            if (lookahead() === TokenType.KW_CASE) {
                const caseToken = parseExpectedTerminal(TokenType.KW_CASE, ParseOptions.withTrivia);
                const caseExpr = parseExpression();
                const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                const body = parseList(ParseContext.switchClause, parseStatement);
                cases.push(Script.SwitchCase(caseToken, caseExpr, colon, body));
            }
            else if (lookahead() === TokenType.KW_DEFAULT) {
                const defaultToken = parseExpectedTerminal(TokenType.KW_DEFAULT, ParseOptions.withTrivia);
                const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                const body = parseList(ParseContext.switchClause, parseStatement);
                cases.push(Script.SwitchDefault(defaultToken, colon, body));
            }
            else {
                parseErrorAtCurrentToken("Expected a switch case or default.");
                break;
            }
        }

        parseContext = savedContext;
        const rightBrace = parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
        return Script.Switch(switchToken, leftParen, expr, rightParen, leftBrace, cases, rightBrace);
    }

    function parseIf() : Script.Conditional {
        const ifToken = parseExpectedTerminal(TokenType.KW_IF, ParseOptions.withTrivia);
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const expr = parseExpression();
        const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        const stmt = parseStatement();

        const root = Script.If(ifToken, leftParen, expr, rightParen, stmt);
        let workingRoot = root;

        while (lookahead() === TokenType.KW_ELSE) {
            const elseToken = parseExpectedTerminal(TokenType.KW_ELSE, ParseOptions.withTrivia);
            const maybeIfToken = parseOptionalTerminal(TokenType.KW_IF, ParseOptions.withTrivia);
            if (maybeIfToken) {
                const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                const expr = parseExpression();
                const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                const stmt = parseStatement();
                workingRoot.alternative = Script.ElseIf(elseToken, maybeIfToken, leftParen, expr, rightParen, stmt);
                workingRoot = workingRoot.alternative!;
            }
            else {
                workingRoot.alternative = Script.Else(elseToken, parseStatement());
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
        let params         : Script.FunctionParameter[];
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
        params     = tryParseFunctionDefinitionParameters(/*speculative*/ false, /*asDeclaration*/ false);
        rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        attrs      = parseTagAttributes();
        body       = parseBracedBlock(ScannerMode.script);

        return Script.FunctionDefinition(accessModifier, returnType, functionToken, nameToken, leftParen, params, rightParen, attrs, body, null);
    }

    function parseNamedFunctionDeclaration() : cfFunctionSignature {
        const decl = tryParseNamedFunctionDefinition(/*speculative*/ false, /*asDeclaration*/ true);
        if (!decl.returnTypeAnnotation) {
            parseErrorAtRange(decl.range, "A function declaration requires a return type.");
        }
        const sig = extractCfFunctionSignature(decl);
        return sig;
    }

    function tryParseNamedFunctionDefinition(speculative: false, asDeclaration: boolean) : Script.FunctionDefinition;
    function tryParseNamedFunctionDefinition(speculative: true) : Script.FunctionDefinition | null;
    function tryParseNamedFunctionDefinition(speculative: boolean, asDeclaration = false) : Script.FunctionDefinition | null {
        let accessModifier: Terminal | null = null;
        let returnType    : DottedPath | null = null;
        let functionToken : Terminal | null;
        let nameToken     : Identifier;
        let leftParen     : Terminal;
        let params        : Script.FunctionParameter[];
        let rightParen    : Terminal;
        let attrs         : TagAttribute[];
        let body          : Block;
        let returnTypeAnnotation : _Type | null = null;
        
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

        nameToken     = parseIdentifier();
        leftParen     = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        params        = tryParseFunctionDefinitionParameters(/*speculative*/ false, asDeclaration);
        
        if (asDeclaration) {
            rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
            /*discarded*/ parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
            returnTypeAnnotation = parseType();

            attrs = [];
            body = Block(null, [], null);
            body.range = new SourceRange(pos(), pos());
        }
        else {
            rightParen    = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
            attrs         = parseTagAttributes();
            body          = parseBracedBlock(ScannerMode.script);
        }

        return Script.FunctionDefinition(accessModifier, returnType, functionToken, nameToken, leftParen, params, rightParen, attrs, body, returnTypeAnnotation);
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
        const savedContext = updateParseContext(ParseContext.for);

        const forToken = parseExpectedTerminal(TokenType.KW_FOR, ParseOptions.withTrivia);
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        let init = isStartOfExpression()
            ? parseAssignmentOrLower()
            : null;

        if (peek().text.toLowerCase() === "in") {
            if (!init || init.kind !== NodeType.variableDeclaration) {
                init = createMissingNode(Identifier(NilTerminal(pos()), ""));
                parseErrorAtRange(leftParen.range.fromInclusive+1, leftParen.range.toExclusive+1, "Declaration expected.");
            }
            const inToken = Terminal(parseNextToken(), parseTrivia());
            const expr = parseExpression();
            const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);

            parseContext = savedContext;
            const body = parseStatement();

            return For.ForIn(forToken, leftParen, init, inToken, expr, rightParen, body);
        }
        else {
            const semi1 = parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);
            const condition = isStartOfExpression() ? parseExpression() : null;
            const semi2 = parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);
            const incrementExpr = isStartOfExpression() ? parseAssignmentOrLower() : null;
            const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);

            parseContext = savedContext;
            const body = parseStatement();
            
            return For.For(forToken, leftParen, init, semi1, condition, semi2, incrementExpr, rightParen, body);
        }
    }

    function parseTry() : Script.Try {
        const tryToken   = parseExpectedTerminal(TokenType.KW_TRY, ParseOptions.withTrivia);
        const leftBrace  = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
        const body       = parseList(ParseContext.blockStatements, parseStatement);
        const rightBrace = parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
        
        const catchBlocks : Script.Catch[] = [];
        while (lookahead() === TokenType.KW_CATCH) {
            const catchToken       = parseExpectedTerminal(TokenType.KW_CATCH, ParseOptions.withTrivia);
            const leftParen        = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
            const exceptionType    = parseDottedPathTypename();
            const exceptionBinding = parseIdentifier();
            const rightParen       = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
            const leftBrace        = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
            const catchBody        = parseList(ParseContext.blockStatements, parseStatement);
            const rightBrace       = (leftBrace.flags & NodeFlags.missing) ? createMissingNode(NilTerminal(pos())) : parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
            catchBlocks.push(
                Script.Catch(catchToken, leftParen, exceptionType, exceptionBinding, rightParen, leftBrace, catchBody, rightBrace));
        }

        let finallyBlock : Script.Finally | null = null;
        if (lookahead() === TokenType.KW_FINALLY) {
            const finallyToken = parseExpectedTerminal(TokenType.KW_FINALLY, ParseOptions.withTrivia);
            const leftBrace = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
            const body = parseList(ParseContext.blockStatements, parseStatement);
            const rightBrace = parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
            finallyBlock = Script.Finally(finallyToken, leftBrace, body, rightBrace);
        }

        return Script.Try(tryToken, leftBrace, body, rightBrace, catchBlocks, finallyBlock);
    }

    function parseList<
        F extends (...v: any) => any,
        Args extends Parameters<F> = Parameters<F>,
        R extends ReturnType<F> = ReturnType<F>>(thisContext: ParseContext, parser: F, ...args: Args) : R[] {
        const result : R[] = [];
        const savedContext = updateParseContext(thisContext);

        while (!endsParseInContext(thisContext)) {
            if (startsParseInContext(thisContext)) {
                result.push(parser(...args as [...Args]) as R);
                continue;
            }

            // no match; can we match with an outer context ?
            // if so, bail
            // otherwise, we're stuck here; so discard the current token and move onto the next
            if (canMatchLookaheadWithOuterContext()) {
                break;
            }
            else {
                parseErrorBasedOnContext(thisContext);
                next(), parseTrivia();
            }
        }

        parseContext = savedContext;
        return result as unknown as R[];
    }

    function parseErrorBasedOnContext(context: ParseContext) {
        switch (context) {
            case ParseContext.typeStruct:
                parseErrorAtCurrentToken("_Type-level struct member definition expected.");
                return;
            case ParseContext.argOrParamList:
                parseErrorAtPos(lastNonTriviaToken.range.toExclusive, "Expression expected.");
                return;
            case ParseContext.structLiteralBody:
                parseErrorAtPos(pos(), "Struct literal key expected.");
                return;
            case ParseContext.cfScriptTagBody:
            case ParseContext.blockStatements:
                parseErrorAtRange(pos(), pos(), "Declaration or statement expected.");
                return;
        }
    }

    function parseBracedBlock(parseBlockInMode: ScannerMode = mode) {
        const savedMode = mode;
        setScannerMode(parseBlockInMode);
        const leftBrace = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
        const body = parseList(ParseContext.blockStatements, parseStatement);

        // if left brace was missing we don't eat right brace
        const rightBrace = (leftBrace.flags & NodeFlags.missing)
            ? createMissingNode(NilTerminal(pos()))
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
                case TokenType.KW_NEW:
                case TokenType.KW_FINAL:
                case TokenType.KW_VAR: {
                    return parseAssignmentOrLower();
                }
                case TokenType.LEFT_BRACE: {
                    // will we *ever* parse a block in tag mode ... ?
                    // anyway, just forward current mode; which is assumed to be script mode, if we're parsing a statement
                    return parseBracedBlock(mode);
                }
                case TokenType.LEFT_BRACKET: {
                    const expr = parseArrayLiteralOrOrderedStructLiteral();
                    const semi = scriptMode() ? parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia) : null;
                    return Statement(expr, semi);
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
                case TokenType.KW_IMPORT: {
                    const importToken = parseExpectedTerminal(TokenType.KW_IMPORT, ParseOptions.withTrivia);
                    const path = parseDottedPathTypename(/*allowTrailingGlob*/ true);
                    const semi = parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);
                    return ImportStatement(importToken, path, semi);
                }
                case TokenType.KW_IF: {
                    return parseIf();
                }
                case TokenType.KW_FUNCTION: {
                    return tryParseNamedFunctionDefinition(/*speculative*/ false, /*asDeclaration*/false);
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
                        parseErrorAtCurrentToken("Unexpected </cf token in a statement context.");
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
                    const peeked = peek();
                    const peekedText = peeked.text.toLowerCase();

                    // special case for sugared `abort`
                    if (peekedText === "abort" && !isInSomeContext(ParseContext.sugaredAbort)) {
                        const terminal = Terminal(parseNextToken(), parseTrivia());
                        if (lookahead() === TokenType.SEMICOLON) {
                            return ScriptSugaredTagCallStatement(terminal, [], parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia));
                        }
                        //
                        // so `function foo() { abort }` is ok, since `}` is not a quote but does not start a statement
                        // whereas `function foo() { abort v + 1 }` is invalid because (v+1) is not a string literal
                        // 
                        else if (isStartOfStatement()) {
                            const statement = doInExtendedContext(ParseContext.sugaredAbort, parseStatement);
                            // we parsed a statement, so any string literal will be wrapped in it
                            // if we got a trivial string value, we are OK
                            // otherwise, emit a diagnostic
                            if (statement.kind === NodeType.statement
                                && (statement.expr!.kind === NodeType.simpleStringLiteral || statement.expr!.kind === NodeType.interpolatedStringLiteral)) {
                                    if (getTriviallyComputableString(statement.expr) !== undefined) {
                                        return ScriptSugaredTagCallStatement(
                                            terminal,
                                            [TagAttribute(Terminal(peek()), "showerror", NilTerminal(pos()), statement.expr!)], // synthesize the "showerror" attribute
                                            parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia));
                                    }
                            }

                            parseErrorAtRange(mergeRanges(terminal, statement), "A sugared abort statement must be followed by a constant string value or a semicolon.");
                            return ScriptSugaredTagCallStatement(
                                /* name */ terminal,
                                /* attrs */ [],
                                /* semicolon */ parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia));
                        }
                        else {
                            return ScriptSugaredTagCallStatement(
                                /* name */ terminal,
                                /* attrs */ [],
                                /* semicolon */ parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia));
                        }
                    }

                    if (isLexemeLikeToken(peeked)) {
                        const maybeFunction = SpeculationHelper.speculate(tryParseNamedFunctionDefinition, /*speculative*/ true);
                        if (maybeFunction) {
                            return maybeFunction;
                        }
                    }
                    
                    if (isSugaredTagName(peekedText)) {
                        const quickPeek = SpeculationHelper.speculate(() => {
                            const sugaredTagNameToken = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/false, /*allowNumeric*/false);
                            return lookahead() === TokenType.LEFT_BRACE
                                ? sugaredTagNameToken
                                : SpeculationHelper.lookahead(() => !!scanTagAttributeName())
                                ? [sugaredTagNameToken, parseTagAttributes()] as const
                                : null;
                        });

                        if (quickPeek) {
                            // if we got an array then we got [terminal, TagAttributes[]]
                            if (Array.isArray(quickPeek)) {
                                if (lookahead() === TokenType.LEFT_BRACE) {
                                    const block = parseBracedBlock();
                                    return ScriptSugaredTagCallBlock(/*name*/quickPeek[0], /*attrs*/[], /*block*/block);
                                }
                                else {
                                    return ScriptSugaredTagCallStatement(quickPeek[0], quickPeek[1], parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia))
                                }
                            }
                            // otherwise we just got a terminal
                            else {
                                const block = parseBracedBlock();
                                return ScriptSugaredTagCallBlock(/*name*/quickPeek as Terminal, /*attrs*/[], /*block*/block);
                            }
                        }
                    }

                    // if the name matches the "cf..." pattern, and the next non trivia is a left paren, this is a TagLikeCall(Statement|Block)
                    // cfSomeTag(attr1=foo, attr2=bar) { ... } (taglikecall block)
                    // cfSomeTag(); (taglikecall statement)
                    if (peekedText.length > 2 && /^cf/i.test(peekedText)) {
                        const nameTerminalIfIsCall = SpeculationHelper.speculate(function() {
                            const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ false);
                            return lookahead() === TokenType.LEFT_PAREN
                                ? name
                                : null;
                        });

                        if (nameTerminalIfIsCall) {
                            const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                            const args = parseList(ParseContext.argOrParamList, parseArgument);
                            const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                            for (let i = 0; i < args.length; i++) {
                                if (!args[i].equals) {
                                    parseErrorAtRange(args[i].expr.range, "Taglike function call arguments must be named.");
                                }
                            }

                            if (lookahead() !== TokenType.LEFT_BRACE) {
                                const semicolon = parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);
                                return ScriptTagCallStatement(nameTerminalIfIsCall, leftParen, args, rightParen, semicolon);
                            }
                            else {
                                const block = parseBracedBlock(mode);
                                return ScriptTagCallBlock(nameTerminalIfIsCall, leftParen, args, rightParen, block);
                            }
                        }
                    }

                    if (isStartOfExpression()) {
                        const result = parseAssignmentOrLower();
                        const semi = scriptMode() ? parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia) : null;
                        return Statement(result, semi);
                    }
                    else {
                        // some unrecoverable error; here we are expecting a statement so we are not inside of any compound expression,
                        // such as an array literal or etc; so this error message is not unreasonable but could probably be improved
                        parseErrorAtRange(peeked.range, "Declaration or statement expected.");
                        
                        // eat the erroneous construct and associated trivia
                        next();
                        parseTrivia();
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
        nilStatement.range = new SourceRange(pos(), pos());
        return nilStatement;
    }

    /**
     * every call to this method clears the last known trivia-bound type annotation,
     * and sets it to the final type annotation of the current trivia collection, if it exists
     */
     function parseTypeAnnotationsFromPreParsedTrivia(trivia: Node | Node[]) : void {
        lastTypeAnnotation = null;

        if (!Array.isArray(trivia)) trivia = [trivia];

        if (trivia.length === 0) {
            return;
        }

        const savedScannerState = getScannerState();

        for (const node of trivia) {
            if (node.kind === NodeType.comment || (node.kind === NodeType.tag && node.tagType === CfTag.TagType.comment)) {
                restoreScannerState({
                    index: node.range.fromInclusive,
                    artificialEndLimit: node.range.toExclusive,
                    mode: ScannerMode.allow_both, // make this optional? it is set in parseTypeAnnotations, too; so we only set it because it is required here
                })

                const types = parseTypeAnnotations(/*asDeclarationFile*/ false);
                
                const typedefs = types.filter(typeShim => typeShim.type.name !== undefined);
                if (typedefs.length > 0) {
                    node.typedefs = typedefs; // store the parsed type functions on the trivia node
                }

                const typeAnnotations = types.filter(typeShim => typeShim.type.name === undefined);
                if (typeAnnotations.length > 1) {
                    // we need to get the type's ranges and terminals and etc.
                    parseErrorAtRange(node.range, "Only one @type annotation is permitted per comment.");
                }

                // <whitespace><comment><whitespace><comment>
                // only the last final comment's type annotation will be considered for the next non-trivial production
                lastTypeAnnotation = typeAnnotations.length === 1 ? typeAnnotations[0].type : null;
            }
        }

        restoreScannerState(savedScannerState);
    }

    /**
     * parse type annotations, 
     */
     function parseTypeAnnotations(asDeclarationFile: boolean) : TypeShim[] {
        setScannerMode(ScannerMode.allow_both);
        const savedContext = updateParseContext(ParseContext.typeAnnotation);
        const result : TypeShim[] = [];
        while (lookahead() !== TokenType.EOF) {
            if (asDeclarationFile) {
                parseTrivia();
            }
            if (next().text === "@") {
                const contextualKeyword = next();
                if (contextualKeyword.text === "declare") {
                    parseTrivia();
                    const declarationSpecifier = peek();
                    if (declarationSpecifier.text === "function") {
                        const functionDecl = parseNamedFunctionDeclaration();
                        result.push(TypeShim(functionDecl));
                    }
                    else if (declarationSpecifier.text === "global") {
                        parseErrorAtRange(contextualKeyword.range, "Global declarations are not yet supported. This would be for the cgi and etc. scopes.");
                        next();
                    }
                    else {
                        parseErrorAtRange(contextualKeyword.range, "Invalid declaration specifier.");
                        next();
                    }
                }
                else if (contextualKeyword.text === "type") {
                    parseTrivia();
                    
                    const nameIfIsTypeConstructorOrAlias = SpeculationHelper.speculate(() => {
                        const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ false);
                        if (lookahead() === TokenType.EQUAL) {
                            return name;
                        }
                        else return null;
                    });

                    //
                    // an assignment generates an alias for the type on the RHS
                    // `@type foo = bar` is an alias declaration
                    // `@type {foo:bar}` is an annotation to be attached to a subsequent node
                    //
                    if (nameIfIsTypeConstructorOrAlias) {
                        parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
                        let type = parseType();
                        (type as Mutable<_Type>).name = nameIfIsTypeConstructorOrAlias.token.text;
                        result.push(TypeShim(type));
                    }
                    // a non-definition is a type-to-term assignment, it will be bound to the next non-trivia/non-type production
                    // like 
                    // <!--- @type Query<Schema> --->
                    // <cfquery name="q"> <!--- q has type `Query<Schema>` --->
                    //
                    else {
                        const type = parseType();
                        result.push(TypeShim(type));
                    }
                }
                else {
                    // parse error ? if skipUnrecognized === false?
                }
            }
        }
        parseContext = savedContext;
        return result;
    }

    function parseType() : _Type;
    function parseType(fromInclusive: number, toExclusive: number) : _Type;
    function parseType(fromInclusive?: number, toExclusive?: number) : _Type {
        function lexemeToType(lexeme: Token) {
            switch (lexeme.text) {
                case "number":
                    return SyntheticType.number;
                case "string":
                    return SyntheticType.string;
                case "any":
                    return SyntheticType.any;
                case "nil":
                    return SyntheticType.nil;
                case "boolean":
                    return SyntheticType.boolean;
                case "true":
                    return SyntheticType.boolean;
                case "false":
                    return SyntheticType.boolean;
                case "void":
                    return SyntheticType.void_;
                default:
                    return cfTypeId(lexeme.text);
            }
        }

        function parseTypeParam() : cfTypeConstructorParam {
            const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/false, /*allowNumeric*/false);
            parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            return cfTypeConstructorParam(name.token.text);
        }

        function parseTypeConstructorInvocation(left: _Type) : _Type {
            parseExpectedTerminal(TokenType.LEFT_ANGLE, ParseOptions.withTrivia);
            const args = parseList(ParseContext.typeParamList, parseTypeListElement);
            parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.withTrivia)
            return cfTypeConstructorInvocation(left, args)
        }

        function maybeParseArrayModifier(type: _Type) : _Type {
            outer:
            while (true) {
                switch (lookahead()) {
                    case TokenType.LEFT_BRACKET:
                        parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                        parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);
                        type = cfArray(type);
                        continue outer;
                    default:
                        break;
                }
                break;
            }
            return type;
        }

        function parseTypeListElement() : _Type {
            const type = parseType();
            parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            return type;
        }

        function parseTypeStructMemberElement() {
            const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ true);
            if (!name) {
                parseErrorAtCurrentToken("Expected a struct key.");
            }

            parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
            const type = parseType();
            parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            return {name, type};
        }

        let savedScannerState : ScannerState | null = null;
        if (fromInclusive) {
            savedScannerState = getScannerState();
            restoreScannerState({
                index: fromInclusive,
                mode: mode,
                artificialEndLimit: toExclusive
            })
        }

        function startsType() {
            switch (lookahead()) {
                case TokenType.LEFT_BRACKET:
                case TokenType.LEFT_PAREN:
                case TokenType.LEFT_BRACE:
                case TokenType.LEFT_ANGLE:
                case TokenType.LEXEME:
                    return true;
                default:
                    return false;
            }
        }

        if (!startsType()) {
            parseErrorAtPos(pos(), "Expected a type expression.");
            return SyntheticType.any;
        }

        parseTrivia(); // only necessary if we just updated scanner state? caller can maybe guarantee that the target scanner state begins on non-trivial input?

        function localParseType() : _Type {
            let result : _Type = SyntheticType.any;

            switch (lookahead()) {
                case TokenType.LEFT_BRACKET: {
                    parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                    const tupleTypes = parseList(ParseContext.typeTupleOrArrayElement, parseTypeListElement);
                    parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);
                    result = maybeParseArrayModifier(cfTuple(tupleTypes));
                    break;
                }
                case TokenType.LEXEME: {
                    result = lexemeToType(next());
                    while (lookahead() === TokenType.LEFT_ANGLE) {
                        result = parseTypeConstructorInvocation(result);
                    }

                    break;
                }
                case TokenType.LEFT_PAREN: {
                    parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                    
                    if (lookahead() === TokenType.LEFT_PAREN) {
                        return parseType();
                    }

                    const parenthesizedType = SpeculationHelper.speculate(() => {
                        const lexeme = next();
                        parseTrivia();
                        if (lookahead() === TokenType.COLON) {
                            // this is a non-type name, used to give a name to an inline function type signature parameter,
                            // i.e, (foo : any) => any
                            //       ^^^^^
                            return null;
                        }
                        else if (lexeme.text === "required") {
                            next();
                            parseTrivia();
                            if (lookahead() === TokenType.COLON) {
                                return null;
                            }
                        }
                        else if (lexeme.type === TokenType.RIGHT_PAREN && peek().type === TokenType.EQUAL_RIGHT_ANGLE) {
                            // got something like `() => `
                            return null;
                        }

                        return lexemeToType(lexeme);
                    })

                    if (parenthesizedType) {
                        parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                        result = maybeParseArrayModifier(parenthesizedType);
                        break;
                    }
                    else {
                        const params = parseFunctionDeclarationParameters();
                        parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                        parseExpectedTerminal(TokenType.EQUAL_RIGHT_ANGLE, ParseOptions.withTrivia);
                        const returnType = parseType();
                        result = maybeParseArrayModifier(cfFunctionSignature("", params, returnType));
                        break;
                    }
                }
                case TokenType.LEFT_BRACE: {
                    parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
                    const kvPairs = parseList(ParseContext.typeStruct, parseTypeStructMemberElement);
                    parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);

                    const members = new Map<string, SymTabEntry>();
                    for (const member of kvPairs) {
                        members.set(member.name.token.text.toLowerCase(), {
                            uiName: member.name.token.text,
                            canonicalName: member.name.token.text.toLowerCase(),
                            declarations: null,
                            type: member.type,
                        })
                    }

                    result = cfStruct(members);
                    result = maybeParseArrayModifier(result);

                    break;
                }
                case TokenType.LEFT_ANGLE: {
                    parseExpectedTerminal(TokenType.LEFT_ANGLE, ParseOptions.withTrivia);
                    const typeParams = parseList(ParseContext.typeParamList, parseTypeParam);
                    parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.withTrivia);
                    parseExpectedTerminal(TokenType.EQUAL_RIGHT_ANGLE, ParseOptions.withTrivia);
                    const body = parseType();
                    result = cfTypeConstructor(typeParams, body);
                    return result; // don't do intersections or unions with type functions
                }
                case TokenType.QUOTE_SINGLE:
                case TokenType.QUOTE_DOUBLE: {
                    const s = parseStringLiteral(/*allowInterpolations*/false);
                    if (s.kind === NodeType.simpleStringLiteral) {
                        result = SyntheticType.string;//cfString(s);
                    }
                    else {
                        result = SyntheticType.string; // should never happen, parseStringLiteral(false) should always return a simpleStringLiteral
                    }
                }
            }

            return result;
        }

        let result = localParseType();

        intersectionsAndUnions:
        while (true) {
            switch (lookahead()) {
                /*
                case TokenType.AMPERSAND:
                    next(), parseTrivia(); // eat ampersand and trivia; if we get the type system kinda working we can save this on the type node
                    result = cfIntersection(result, localParseType());
                    continue intersectionsAndUnions;
                */
                case TokenType.PIPE: {
                    next(), parseTrivia();
                    if (isUnion(result)) {
                        result.types.push(localParseType());
                    }
                    else {
                        result = cfUnion(result, localParseType());
                    }
                }
                default: {
                    break intersectionsAndUnions;
                }
            }
        }

        if (savedScannerState) {
            restoreScannerState(savedScannerState);
        }

        return result;
    }

    function parseScriptTriviaWithPossibleTypeAnnotation() : {trivia: Node[], type: _Type | null} {
        const trivia : Node[] = [];
        let type : _Type | null = null;

        const savedContext = updateParseContext(ParseContext.trivia);

        while (lookahead() !== TokenType.EOF) {
            if (lookahead() === TokenType.WHITESPACE) {
                trivia.push(TextSpan(next().range, ""));
            }
            else if (lookahead() === TokenType.FORWARD_SLASH_STAR) {
                if (peek(1).type === TokenType.COLON) {
                    if (type !== null) {
                        parseErrorAtCurrentToken("A comment containing a type annotation must contain exactly one type annotation.");
                        trivia.push(...parseTrivia());
                    }
                    else {
                        const commentStart = pos();
                        scanToNextToken([TokenType.STAR_FORWARD_SLASH], /*endOnOrAfter*/ "after");
                        const commentEnd = pos();

                        type = parseType(commentStart+3, commentEnd-2);

                        trivia.push(Comment(CommentType.scriptMultiLine, new SourceRange(commentStart, commentEnd)));
                    }
                }
            }
            else { 
                break;
            }
        }

        parseContext = savedContext;

        return {trivia, type};
    }
}
