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
    OrderedStructLiteral, } from "./node";
import { SourceRange, Token, TokenType, TokenizerMode, Scanner, TokenTypeUiString } from "./scanner";
import { allowTagBody, isLexemeLikeToken, requiresEndTag, getAttributeValue, getTriviallyComputableBoolean, getTriviallyComputableString, isNamedBlockName } from "./utils";

const enum ParseOptions {
    none     = 0,
    noTrivia = 0,
    withTrivia        = 0x00000001,
    allowHashWrapped  = 0x00000002,
};

const enum ParseContext {
    none = 0,
    hashWrappedExpr  = 0x00000001,
    cfScriptTagBody  = 0x00000002,
    for              = 0x00000004,
    interpolatedText = 0x00000008,
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
    mode : TokenizerMode;
    index: number;
    artificialEndLimit: number | undefined;
}

export interface Diagnostic {
    fileName: string;
    fromInclusive: number;
    toExclusive: number;
    msg: string;
}

// @fixme: don't take params, use a "setMode()", "setScanner", etc.
export function Parser() {
    function setScanner(scanner_: Scanner, mode_: TokenizerMode) {
        scanner = scanner_;
        mode = mode_;
        lookahead_ = peek().type;
        parseContext = ParseContext.none;
        return self_;
    }
    function setMode(mode_: TokenizerMode) {
        mode = mode_;
        return self_;
    }
    function setDebug(isDebug: boolean) {
        setNodeFactoryDebug(isDebug);
        return self_;
    }

    let scanner : Scanner;
    let mode: TokenizerMode;
    let parseContext : ParseContext;
    let lookahead_ : TokenType;
    
    let globalDiagnosticEmitter : (() => void) | null = null;

    const diagnostics : Diagnostic[] = [];
    let parseErrorBeforeNextFinishedNode = false;
    parseErrorBeforeNextFinishedNode;

    const SpeculationHelper = (function() {
        //
        // run a boolean returning worker, and always rollback changes to parser state when done
        //
        function lookahead(lookaheadWorker: () => boolean) {
            const saveTokenizerState = getTokenizerState();
            const saveParseErrorBeforeNextFinishedNode = parseErrorBeforeNextFinishedNode;

            const result = lookaheadWorker();

            restoreTokenizerState(saveTokenizerState);
            parseErrorBeforeNextFinishedNode = saveParseErrorBeforeNextFinishedNode;
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
            const saveParseErrorBeforeNextFinishedNode = parseErrorBeforeNextFinishedNode;

            const result = speculationWorker(...args as [...Args]);

            if (result) {
                return result;
            }
            else {
                parseErrorBeforeNextFinishedNode = saveParseErrorBeforeNextFinishedNode;
                restoreTokenizerState(saveTokenizerState);
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
        setMode,
        setDebug,
        parseTags,
        getDiagnostics
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
        return mode === TokenizerMode.tag;
    }
    function scriptMode() : boolean {
        return mode === TokenizerMode.script;
    }
    function isInSomeContext(contextFlags: ParseContext) {
        return (parseContext & contextFlags) === contextFlags;
    }
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
        parseErrorBeforeNextFinishedNode = true;
        diagnostics.push({
            fileName: "<NYI>",
            fromInclusive: typeof fromInclusiveOrRange === "number"
                ? fromInclusiveOrRange
                : fromInclusiveOrRange.fromInclusive,
            toExclusive: typeof toExclusiveOrMsg === "number"
                ? toExclusiveOrMsg
                : (fromInclusiveOrRange as SourceRange).toExclusive,
            msg: msg ?? (toExclusiveOrMsg as string)
        });
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
                parseErrorAtCurrentToken("Expected a " + TokenTypeUiString[type] + " here");
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
        else if (!/^[_a-z]+$/i.test(token.text)) {
            parseErrorAtRange(token.range, "Invalid cftag name");
        }

        const trivia = parseTrivia();
        const forcedLexemeToken = Token(TokenType.LEXEME, token.text, token.range.fromInclusive, token.range.toExclusive);
        return Terminal(forcedLexemeToken, trivia);
    }

    function parseExpectedLexemeLikeTerminal(consumeOnFailure: boolean, errorMsg?: string) : Terminal {
        const labelLike = peek();
        let trivia : Node[] = [];
        if (!isLexemeLikeToken(labelLike)) {
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
        const commentBody = parseTagTrivia();
        const commentEnd = parseExpectedTerminal(TokenType.CF_TAG_COMMENT_END, ParseOptions.noTrivia);

        // if no comment end, emit an error

        return CfTag.Comment(commentStart, commentBody, commentEnd);
    }

    function parseScriptSingleLineComment() : Comment {
        throw "nyi";
    }

    function parseScriptMultiLineComment() : Comment {
        const startToken = parseExpectedTerminal(TokenType.FORWARD_SLASH_STAR, ParseOptions.noTrivia);
        while (lookahead() !== TokenType.STAR_FORWARD_SLASH && lookahead() !== TokenType.EOF) {
            next();
        }
        const endToken = parseExpectedTerminal(TokenType.STAR_FORWARD_SLASH, ParseOptions.noTrivia);
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

        if (canonicalName === "if" || canonicalName === "elseif") {
            const expr = parseExpression();
            const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
            return CfTag.ScriptLike(CfTag.Which.start, tagStart, tagName, null, rightAngle, canonicalName, expr);
        }
        else if (canonicalName === "set") {
            const expr = parseAssignmentOrLower();
            const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
            return CfTag.ScriptLike(CfTag.Which.start, tagStart, tagName, null, rightAngle, canonicalName, expr);
        }
        else {
            const tagAttrs = parseTagAttributes();
            const maybeVoidSlash = parseOptionalTerminal(TokenType.FORWARD_SLASH, ParseOptions.withTrivia);
            const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
            if (canonicalName === "script") {
                const saveMode = mode;
                mode = TokenizerMode.script;

                // hm, probably mode should be a context flag, too
                const stmtList = doInContext(ParseContext.cfScriptTagBody, parseStatementList);

                mode = saveMode;
                return CfTag.Script(tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, stmtList);
            }
            else {
                return CfTag.Common(CfTag.Which.start, tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, tagAttrs);
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

        while (lookahead() === TokenType.LEXEME) {
            const attrName = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
            if (lookahead() === TokenType.EQUAL) {
                const equal = parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
                let value : Node;
                if (lookahead() === TokenType.LEXEME) {
                    value = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
                }
                else {
                    value = parseExpression();
                }
                result.push(TagAttribute(attrName, scanner.getTokenText(attrName.token).toLowerCase(), equal, value));
            }
            else {
                result.push(TagAttribute(attrName, scanner.getTokenText(attrName.token).toLowerCase()));
            }
        }

        return result;
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

                parseErrorAtRange(root.range, "Missing </cfif> tag");
                return root;
            }

            function treeifyTagFunction(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) {
                function parseParam(tag: CfTag.Common) : FunctionParameter {
                    const nameAttr = getAttributeValue(tag.attrs, "name");
                    let name = "";
                    if (!nameAttr) {
                        parseErrorAtRange(tag.range, "<cfargument> requires a 'name' attribute");
                    }
                    else {
                        const nameVal = getTriviallyComputableString(nameAttr);
                        if (!nameVal) {
                            parseErrorAtRange(nameAttr.range, "<cfargument> 'name' attribute must be a constant string value");
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
                            parseErrorAtRange(requiredAttr.range, "<cfargument> 'required' attribute does not allow coercions to boolean from negative numbers");
                            isRequired = false;
                        }
                        else {
                            const boolVal = getTriviallyComputableBoolean(requiredAttr);
                            if (boolVal === undefined) { // whatever the value was, it was not coercible to bool
                                parseErrorAtRange(requiredAttr.range, "<cfargument> 'required' attribute must be a constant boolean value");
                            }
                            isRequired = boolVal ?? false;
                        }
                    }

                    return FromTag.FunctionParameter(tag, name, isRequired, /*FIXME*/ null);
                }

                let functionName = "";
                const functionNameExpr = getAttributeValue(startTag.attrs, "name");
                if (!functionNameExpr) {
                    parseErrorAtRange(startTag.range, "<cffunction> requires a name attribute")
                }
                else {
                    let functionName = getTriviallyComputableString(functionNameExpr);
                    if (!functionName) {
                        parseErrorAtRange(functionNameExpr.range, "<cffunction> name attribute must be a constant")
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
                                const expr = (<CfTag.ScriptLike>tag).expr;
                                expr.tagOrigin.startTag = tag;
                                result.push(expr);
                                nextTag();
                                continue;
                            }
                            case "function": {
                                openTagStack.push("function");
                                const startTag = parseExpectedTag(CfTag.Which.start, "function");
                                const body = treeifyTags(stopAt(CfTag.Which.end, "function", ReductionScheme.return));
                                const endTag = parseExpectedTag(CfTag.Which.end, "function", () => parseErrorAtRange(startTag.range, "Missing </cffunction> tag"))
                                openTagStack.pop();
                                result.push(treeifyTagFunction(startTag as CfTag.Common, body, endTag as CfTag.Common));
                                continue;
                            }
                            case "script": {
                                nextTag();
                                const endTag = parseExpectedTag(CfTag.Which.end, "script", () => parseErrorAtRange(tag.range, "Missing </cfscript> tag"));
                                result.push(
                                    FromTag.Block(
                                        tag,
                                        (<CfTag.Script>tag).stmtList,
                                        endTag));
                                continue;
                            }
                            default: {
                                if (requiresEndTag(tag)) {
                                    openTagStack.push(tag.canonicalName);

                                    const startTag = tag;
                                    nextTag();
                                    const blockChildren = treeifyTags(stopAt(CfTag.Which.end, startTag.canonicalName, ReductionScheme.return));

                                    openTagStack.pop();

                                    const endTag = parseExpectedTag(CfTag.Which.end, startTag.canonicalName, () => parseErrorAtRange(startTag.range, "Missing </cf" + startTag.canonicalName + ">"));
                                    updateTagContext(endTag);
                                    result.push(FromTag.Block(startTag, blockChildren, endTag));
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
                                parseErrorAtRange(tag.range, "End tag without a matching start tag (cf" + tag.canonicalName + ")")
                            }
                        }

                        nextTag();
                    }
                }

                return result;
            }

            return treeifyTags(reductionInstructions.default);
        }

        const saveMode = mode;
        mode = TokenizerMode.tag;

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

        mode = saveMode;

        return treeifyTagList(result);
    }

    function parseTextInTagContext(range: SourceRange, tagContext: TagContext) {
        if (!tagContext.inTextInterpolationContext()) {
            return TextSpan(range, scanner.getTextSlice(range));
        }
        const saveTokenizerState = getTokenizerState();
        restoreTokenizerState({
            index: range.fromInclusive,
            mode: TokenizerMode.tag,
            artificialEndLimit: range.toExclusive});
        
        const result = parseTextWithPossibleInterpolations();
        restoreTokenizerState(saveTokenizerState);
        return result;
    }

    function parseTextWithPossibleInterpolations(quoteDelimiter?: TokenType.QUOTE_SINGLE | TokenType.QUOTE_DOUBLE) : (TextSpan | HashWrappedExpr)[] {
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
            if (finalModifier || varModifier) {
                // we could probably return a "declaration" here ?
                // var x;  sure it's semantically meaningless in cf (all var declarations require initializers), but it looks valid
                // would also be useful in a for-in: `for (var x in y)` where `var x` is a declaration
                // so we could check if we're in a for-in context
                parseErrorAtRange(root.range, "variable declarations require initializers");
                return root;
            }
            else {
                // just a plain ol' expr
                return root;
            }
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

    function parseExpression(descendIntoOr = true) : Node { // i think this binds the &&'s correctly
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

        return root;
    }

    function parseComparisonExpressionOrLower() : Node {
        const saveDiagnosticEmitter = globalDiagnosticEmitter;
        const currentPos = scanner.getIndex();
        globalDiagnosticEmitter = () => parseErrorAtRange(currentPos, scanner.getIndex(), "Expected an expression");

        let root = parseAddition();

        while (true) {
            if (tagMode() && lookahead() === TokenType.LEFT_ANGLE || lookahead() === TokenType.RIGHT_ANGLE) {
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

        globalDiagnosticEmitter = saveDiagnosticEmitter;

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

        while (true) {
            switch (lookahead()) {
                case TokenType.STAR:
                case TokenType.FORWARD_SLASH: {
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
                const expr = parseExpression();
                const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);

                // peek for indexed-access or call expression here?
                // if lookahead() == dot or left brace or paren, its one of those
                // now, those are illegal;
                // but easily recognizable

                return Parenthetical(leftParen, expr, rightParen);
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
            expr = parseExpression();
        }
        else {
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

        let root : Node = parseIdentifier(ParseOptions.allowHashWrapped);
        if (root.type === NodeType.hashWrappedExpr) {
            return root;
        }

        while (lookahead() != TokenType.EOF) {
            switch(lookahead()) {
                case TokenType.LEFT_BRACKET:
                case TokenType.DOT:
                    root = parseIndexedAccess(root);
                    continue;
                case TokenType.LEFT_PAREN:
                    root = parseCallExpression(root);
                    continue;
            }
            break;
        }

        switch (lookahead()) {
            case TokenType.DBL_PLUS:
            case TokenType.DBL_MINUS:
                const unaryOp = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                root = UnaryOperator(root, unaryOp);
        }

        return root;
    }

    function parseIndexedAccess(root: Node) : Node {
        while (lookahead() != TokenType.EOF) {
            switch (lookahead()) {
                case TokenType.LEFT_BRACKET: {
                    const leftBracket = parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                    const expr = parseExpression();
                    const rightBracket = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);

                    if (root.type !== NodeType.indexedAccess) {
                        root = IndexedAccess(root);
                    }
                    pushAccessElement(root, leftBracket, expr, rightBracket);

                    continue;
                }
                case TokenType.DOT: {
                    const dot = parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
                    const propertyName = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
                    if (root.type !== NodeType.indexedAccess) {
                        root = IndexedAccess(root);
                    }
                    pushAccessElement(root, dot, propertyName);
                    continue;
                }
            }
            break;
        }
        return root;
    }

    function parseCallExpression(root: Node) {
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const args : CallArgument[] = [];
        while (lookahead() != TokenType.EOF && lookahead() != TokenType.RIGHT_PAREN) {
            const expr = parseExpression();
            const comma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            args.push(CallArgument(expr, comma));
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
                    const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true);
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
        return StructLiteral(leftBrace, kvPairs, rightBrace);
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
            return EmptyOrderedStructLiteral(leftBracket, colon, rightBracket);
        }        

        function isExpressionThenColon() {
            parseExpression();
            return lookahead() === TokenType.COLON;
        }

        if (SpeculationHelper.lookahead(isExpressionThenColon)) {
            const structMembers : StructLiteralInitializerMember[] = [];
            while (lookahead() !== TokenType.RIGHT_BRACKET && lookahead() !== TokenType.EOF) {
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
            return OrderedStructLiteral(leftBracket, structMembers, rightBracket);
        }
        else {
            const elements : ArrayLiteralInitializerMember[] = [];
            while (lookahead() !== TokenType.RIGHT_BRACKET && lookahead() !== TokenType.EOF) {
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
            return ArrayLiteral(leftBracket, elements, rightBracket);
        }
    }

    function parseIdentifier() : Identifier;
    function parseIdentifier(parseOptions: ParseOptions.allowHashWrapped) : Identifier | HashWrappedExpr;
    function parseIdentifier(parseOptions?: ParseOptions.allowHashWrapped) : Identifier | HashWrappedExpr {
        let leftHash : Terminal | null = null;
        if (parseOptions && (parseOptions & ParseOptions.allowHashWrapped) && !isInSomeContext(ParseContext.hashWrappedExpr)) {
            leftHash = parseOptionalTerminal(TokenType.HASH, ParseOptions.withTrivia);
        }

        const identifier = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true);

        // @fixme: make sure the identifier name is valid
        // for example, "var" is a valid identifier, but "and" and "final" are not
        // in cf2021 they tightened it a bit, so 'function' is no longer a valid identifier, probably others
        
        const canonicalName = identifier.token.text.toLowerCase();

        if (leftHash && parseOptions! & ParseOptions.allowHashWrapped) {
            const rightHash = parseExpectedTerminal(
                TokenType.HASH,
                ParseOptions.withTrivia,
                () => parseErrorAtRange(
                    leftHash!.range.fromInclusive,
                    scanner.getIndex(),
                    "Unterminated hash wrapped expression"));

            return HashWrappedExpr(
                leftHash,
                Identifier(identifier, canonicalName),
                rightHash);
        }
        else {
            return Identifier(identifier, canonicalName);
        }
    }

    function parseStringLiteral() : Node {
        const quoteType = lookahead();
        if (quoteType !== TokenType.QUOTE_SINGLE && quoteType !== TokenType.QUOTE_DOUBLE) {
            // will a lookahead or speculate ever trigger this ... ?
            throw "AssertionFailure: parseStringLiteral called on input without valid string delimiter";
        }

        const leftQuote = parseExpectedTerminal(quoteType, ParseOptions.noTrivia);
        const stringElements = parseTextWithPossibleInterpolations(quoteType);
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
        if (lookahead() !== TokenType.LEXEME) return false;
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
        while (lookahead() !== TokenType.RIGHT_PAREN && lookahead() !== TokenType.EOF) {
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
        const saveMode = mode;
        if (lookahead() === TokenType.LEFT_BRACE) {
            mode = TokenizerMode.script;
        }
        const stmtOrBlock = parseStatement();
        mode = saveMode;

        return ArrowFunctionDefinition(leftParen, params, rightParen, fatArrow, stmtOrBlock);
    }

    function parseSwitch() {
        const switchToken = parseExpectedTerminal(TokenType.KW_SWITCH, ParseOptions.withTrivia);
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const expr = parseExpression();
        const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        const leftBrace = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);

        const cases : SwitchCase[] = [];

        while (lookahead() !== TokenType.EOF) {
            let thisCase : SwitchCase;
            if (lookahead() === TokenType.RIGHT_BRACE) {
                break;
            }
            else if (lookahead() === TokenType.KW_CASE) {
                const caseToken = parseExpectedTerminal(TokenType.KW_CASE, ParseOptions.withTrivia);
                const caseExpr = parseExpression();
                const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                thisCase = SwitchCase.Case(caseToken, caseExpr, colon, []);
            }
            else if (lookahead() === TokenType.KW_DEFAULT) {
                const defaultToken = parseExpectedTerminal(TokenType.KW_DEFAULT, ParseOptions.withTrivia);
                const colon = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                thisCase = SwitchCase.Default(defaultToken, colon, []);
            }
            else {
                parseErrorAtCurrentToken("Expected a switch case or default here");
                break;
            }

            // eat statements up to the switch statement's closing brace or the next case
            while (lookahead() !== TokenType.KW_CASE && lookahead() !== TokenType.KW_DEFAULT && lookahead() !== TokenType.RIGHT_BRACE) {
                thisCase.statements.push(parseStatement());
            }
            cases.push(thisCase);
        }

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
            const maybeIfToken = parseExpectedTerminal(TokenType.KW_IF, ParseOptions.withTrivia);
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
        body       = parseBlock(TokenizerMode.script);

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
            accessModifier = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true);
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

        nameToken     = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false);
        leftParen     = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        params        = tryParseFunctionDefinitionParameters(/*speculative*/ false);
        rightParen    = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        attrs         = parseTagAttributes();
        body          = parseBlock(TokenizerMode.script);

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
        const init = parseAssignmentOrLower();
        if (peek().text.toLowerCase() === "in") {
            const inToken = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false);
            const expr = parseExpression();
            const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
            const body = parseStatement();
            return For.ForIn(forToken, leftParen, init, inToken, expr, rightParen, body);
        }
        else {
            const semi1 = parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);
            const condition = parseExpression();
            const semi2 = parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);
            const incrementExpr = parseExpression();
            const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
            const body = parseStatement();
            return For.For(forToken, leftParen, init, semi1, condition, semi2, incrementExpr, rightParen, body);
        }
    }

    function parseStatementList() : Node[] {
        const result : Node[] = [];

        while (true) {
            if (lookahead() === TokenType.RIGHT_BRACE || lookahead() === TokenType.EOF) {
                break;
            }
            if (isInSomeContext(ParseContext.cfScriptTagBody)) {
                if (lookahead() === TokenType.CF_END_TAG_START) {
                    break;
                }
            }

            result.push(parseStatement());
        }

        return result;
    }

    function parseBlock(parseBlockInMode: TokenizerMode) {
        const savedMode = mode;
        mode = parseBlockInMode;
        const leftBrace = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
        const body = parseStatementList();
        const rightBrace = parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
        mode = savedMode;
        return Block(leftBrace, body, rightBrace);
    }

    function parseStatement() : Node {
        outer:
        while (lookahead() !== TokenType.EOF) {
            switch (lookahead()) {
                case TokenType.WHITESPACE: {
                    // @fixme: attach this to a node somehow to get a beter round-trip
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
                case TokenType.NUMBER: {
                    const expr = parseExpression();
                    const semicolon = scriptMode() ? parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia) : null;
                    return Statement(expr, semicolon);
                }
                case TokenType.KW_SWITCH: {
                    return parseSwitch();
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
                default: {
                    const peeked = peek();
                    if (isLexemeLikeToken(peeked)) {
                        const maybeFunction = SpeculationHelper.speculate(tryParseNamedFunctionDefinition, /*speculative*/ true);
                        if (maybeFunction) {
                            return maybeFunction;
                        }
                    }

                    if (isNamedBlockName(peeked.text)) {
                        const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true);
                        const attrs = parseTagAttributes();
                        const block = parseBlock(mode);
                        return NamedBlockFromBlock(name, attrs, block);
                    }
                    else {
                        return parseAssignmentOrLower();
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
