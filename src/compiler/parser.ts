import {
    Diagnostic,
    CfTag, Node, NodeKind, TagAttribute, NodeFlags, Terminal, Comment, TextSpan, NilTerminal,
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
    ScriptSugaredTagCallStatement, ScriptTagCallStatement, SourceFile, Script, Tag, SpreadStructLiteralInitializerMember, StructLiteralInitializerMember, SimpleArrayLiteralInitializerMember, SpreadArrayLiteralInitializerMember, SliceExpression, SymTabEntry, pushDottedPathElement, ParamStatementWithImplicitTypeAndName, ParamStatementWithImplicitName, ParamStatement, ShorthandStructLiteralInitializerMember, DiagnosticKind, Typedef, Interfacedef, TypeShimKind, TypeAnnotation, NamedAnnotation, NonCompositeFunctionTypeAnnotation, StaticAccess, Namespace, TypeImport, TypeInfo } from "./node";
import { SourceRange, Token, TokenType, ScannerMode, Scanner, TokenTypeUiString, CfFileType } from "./scanner";
import { allowTagBody, isLexemeLikeToken, requiresEndTag, getTriviallyComputableString, isSugaredTagName, isSimpleOrInterpolatedStringLiteral, getAttributeValue, stringifyDottedPath, exhaustiveCaseGuard, Mutable } from "./utils";
import { cfIndexedType, Interface, isStructLike, TypeConstructorParam, TypeConstructorInvocation, CfcLookup, createLiteralType, TypeConstructor, IndexSignature, TypeKind, isUninstantiatedArray, cfTypeConstructorParam, cfGenericFunctionSignature, cfConditionalType, cfInterpolatedString, cfInferenceTarget, cfTuple, cfTypeConstructorInvocation } from "./types";
import { Type, UninstantiatedArray, Struct, cfFunctionSignature, cfTypeId, cfUnion, BuiltinType, cfFunctionSignatureParam } from "./types";
import { Engine } from "./engines";
import { ProjectOptions } from "./project";

const enum ParseOptions {
    none     = 0,
    noTrivia = 0,
    withTrivia        = 0x00000001,
    allowHashWrapped  = 0x00000002,
};

enum StringUniverse { value = 1, type = 2 }

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
    interface,
    cfcPsuedoConstructor, // inside the top-level of a cfc
    typeExtends,
    typeString,
    typeNamespace,
    END                 // sentinel for looping over ParseContexts
}

function TagContext() {
    const depth = {
        output: 0,
        mail: 0,
        query: 0,
        function: 0
    };
    
    function inTextInterpolationContext() {
        return depth.output > 0
            || depth.mail > 0
            || depth.query > 0;
    };

    function inFunction() {
        return depth.function > 0;
    }

    function update(tag: CfTag) {
        const bumpDir = tag.which === CfTag.Which.start ? 1 : -1;
        switch (tag.canonicalName as keyof typeof depth) {
            case "output":
            case "mail":
            case "query":
            case "function":
                depth[tag.canonicalName as keyof typeof depth] += bumpDir;
            default:
                // no-op
        }
    }

    return {
        inTextInterpolationContext,
        inFunction,
        update
    }
}

type TagContext = ReturnType<typeof TagContext>;

interface ScannerState {
    mode : ScannerMode;
    index: number;
    artificialEndLimit: number | undefined;
}

export function Parser(config: ProjectOptions) {  
    const parseTypes = config.parseTypes;
    const debug = config.debug;
    const engineVersion = config.engineVersion;

    function primePeekCacheAndLookahead() : void {
        peek0Cache_ = null;
        const r = peek();
        lookahead_ = r.type;
    }

    function setScannerMode(mode_: ScannerMode) {
        mode = mode_;
        primePeekCacheAndLookahead();
        return self_;
    }

    let scanner : Scanner;
    let sourceFile: SourceFile;
    let mode: ScannerMode;
    let parseContext : ParseContext;
    let lookahead_ : TokenType;
    let token_ : Token;
    let peek0Cache_ : {mode: ScannerMode, value: Token} | null = null;
    let lastNonTriviaToken : Token | undefined;
    let diagnostics : Diagnostic[] = [];
    let typedefContainer! : Node;
    let lastDocBlock : {
        typeAnnotation: TypeAnnotation | null,
        typedefs: (Typedef | Interfacedef | Namespace | TypeImport)[],
        argumentAnnotations: NamedAnnotation[] | null,
        docBlockAttrs: TagAttribute[],
        treatAsConstructor: boolean 
    } | null = null;
    let parseErrorMsg : string | null = null;

    const SpeculationHelper = (function() {
        //
        // run a `T` returning worker, and always rollback changes to parser state when done
        //
        function lookahead<T>(lookaheadWorker: () => T) {
            const saveTokenizerState = getScannerState();
            const diagnosticsLimit = diagnostics.length;
            const savedLastNonTriviaToken = lastNonTriviaToken;
            const savedLastDocBlock = lastDocBlock;

            const result = lookaheadWorker();

            diagnostics.splice(diagnosticsLimit); // drop any diagnostics that were added
            restoreScannerState(saveTokenizerState);
            lastNonTriviaToken = savedLastNonTriviaToken;
            lastDocBlock = savedLastDocBlock;
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
            const savedLastDocBlock = lastDocBlock;

            const result = speculationWorker(...args as [...Args]);

            if (result) {
                return result;
            }
            else {
                restoreScannerState(saveTokenizerState);
                parseErrorMsg = savedParseErrorMsg;
                diagnostics.splice(diagnosticsLimit); // drop any diagnostics that were added
                lastNonTriviaToken = savedLastNonTriviaToken;
                lastDocBlock = savedLastDocBlock;
                return null;
            }
        }
        return {
            lookahead,
            speculate
        }
    })();

    const self_ = {
        parseTags,
        parseScript,
        parse,
    };

    return self_;

    /*********************************
    /* impl
    /********************************/
    function peek(jump: number = 0) : Token {
        if (jump === 0 && peek0Cache_ !== null && peek0Cache_.mode === mode) {
            return peek0Cache_.value;
        }

        const r = scanner.peek(jump, mode);

        if (jump === 0) {
            peek0Cache_ = {mode: mode, value: r};
        }

        return r;
    }

    function peekChar(jump: number = 0): string {
        return scanner.peekChar(jump);
    }

    function advance(jump: number) : void {
        scanner.advance(jump);
        primePeekCacheAndLookahead();
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
        let result : Token;
        if (peek0Cache_ !== null && peek0Cache_.mode === mode) {
            scanner.advance(peek0Cache_.value.range.size())
            result = peek0Cache_.value;
        }
        else {
            result = scanner.nextToken(mode);
        }
        token_ = result;
        primePeekCacheAndLookahead();
        if (!isInSomeContext(ParseContext.trivia)) {
            lastNonTriviaToken = result;
        }
        return result;
    }

    function scanToNextToken(token: TokenType[], endOnOrAfter: "on" | "after" = "on") : void {
        scanner.scanToNext(token, mode);
        if (endOnOrAfter === "after") {
            scanner.nextToken(mode);
        }
        primePeekCacheAndLookahead();
    }

    function scanToNextChar(char: string, endOnOrAfter: "on" | "after" = "on") : void {
        scanner.scanToNext(char);
        if (endOnOrAfter === "after") {
            scanner.advance();
        }
        primePeekCacheAndLookahead();
    }

    function scanTagName() : Token | null {
        const result = scanner.scanTagName();
        if (result && !isInSomeContext(ParseContext.trivia)) lastNonTriviaToken = result;
        primePeekCacheAndLookahead();
        return result;
    }

    function scanTagAttributeName(allowDot = false) {
        const result = scanner.scanTagAttributeName(allowDot);
        if (result && !isInSomeContext(ParseContext.trivia)) lastNonTriviaToken = result;
        primePeekCacheAndLookahead();
        return result;
    }

    function scanToNextTagCommentToken() {
        scanner.scanToNextTagCommentToken();
        primePeekCacheAndLookahead();
    }

    function scanLexemeLikeStructKey() : Token | null {
        const result = scanner.scanLexemeLikeStructKey();
        if (result && !isInSomeContext(ParseContext.trivia)) lastNonTriviaToken = result;
        primePeekCacheAndLookahead();
        return result;
    }

    function isIdentifier() : boolean {
        return scanner.isIdentifier();
    }

    function scanIdentifier() : Token | null {
        const result = scanner.scanIdentifier();
        if (result && !isInSomeContext(ParseContext.trivia)) lastNonTriviaToken = result;
        primePeekCacheAndLookahead();
        return result;
    }

    function scanDocBlockAttrName() : string | null {
        const result = scanner.scanDocBlockAttrName();
        primePeekCacheAndLookahead();
        return result;
    }

    function scanDocBlockAttrText() : string {
        const result = scanner.scanDocBlockAttrText();
        primePeekCacheAndLookahead();
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
        primePeekCacheAndLookahead();
    }

    function tagMode() : boolean {
        return !!(mode & ScannerMode.tag);
    }
    function scriptMode() : boolean {
        return !!(mode & ScannerMode.script);
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
     * updates the context to be extended with a new context, and returns the previous context
     */
    function updateParseContext(newContext: ParseContext) {
        const savedContext = parseContext;
        parseContext |= (1 << newContext);
        return savedContext;
    }

    /**
     * updates the context to exclude a context flag, and returns the previous context
     */
    function dropParseContext(context: ParseContext) {
        const savedContext = parseContext;
        parseContext &= ~(1 << context);
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
            kind: DiagnosticKind.error,
            fromInclusive: from,
            toExclusive: to,
            msg: msg ?? (toExclusiveOrMsg as string)
        };

        if (debug) {
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

    function phonyTerminalFromCurrentPos(type: TokenType) {
        const errorPos = pos();
        const emptyRange = new SourceRange(errorPos, errorPos);
        const phonyToken : Token = Token(type, "", emptyRange);
        return createMissingNode(Terminal(phonyToken));
    }

    function parseExpectedTerminal(type: TokenType, parseOptions: ParseOptions, errorMsg?: string) : Terminal {
        const maybeTerminal = parseOptionalTerminal(type, parseOptions);
        if (maybeTerminal) {
            return maybeTerminal;
        }
        else {
            const actualError = errorMsg ?? "Expected '" + TokenTypeUiString[type] + "'";
            const errPos = lastNonTriviaToken?.range.toExclusive ?? 0;
            parseErrorAtPos(errPos, actualError);
            return phonyTerminalFromCurrentPos(type);
        }
    }

    function parseExpectedTagName() {
        const tagName = scanTagName();

        if (!tagName) {
            parseErrorAtCurrentToken("Expected a tag name.");
            return createMissingNode(Terminal(next()));
        }

        return Terminal(tagName, parseTrivia());
    }

    /**
     * the other parseExpectedLexemeLike terminal has a "consumeOnFailure" but it doesn't
     * we shouldn't need two implementations, but need to see what breaks if we get rid of the other one
     */
    function parseExpectedLexemeLikeTerminal_alwaysAdvanceOnFaliure(parseOptions: ParseOptions, allowLeadingNumeric = false, errMsg?: string) {
        if (isLexemeLikeToken(peek(), allowLeadingNumeric)) {
            const result = Terminal(next());
            if (parseOptions === ParseOptions.withTrivia) {
                result.trivia = parseTrivia();
            }
            return result;
        }
        else {
            const result = phonyTerminalFromCurrentPos(TokenType.LEXEME);
            if (errMsg) {
                parseErrorAtCurrentToken(errMsg);
            }
            next();
            return result;
        }
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
            const commentEnd = createMissingNode(Terminal(next()));
            result = CfTag.Comment(commentStart, nestedComments, commentEnd);
        }
        else {
            const commentEnd = parseExpectedTerminal(TokenType.CF_TAG_COMMENT_END, ParseOptions.noTrivia);
            result = CfTag.Comment(commentStart, nestedComments, commentEnd);
        }

        if (parseTypes && !isInSomeContext(ParseContext.typeAnnotation)) {
            parseTypeAnnotationsFromPreParsedTrivia(result);
        }

        return result;
    }

    function parseScriptSingleLineComment() : Comment {
        const start = parseExpectedTerminal(TokenType.DBL_FORWARD_SLASH, ParseOptions.noTrivia);
        scanToNextChar("\n", /*endOnOrAfter*/"after");

        const comment = Comment(CommentType.scriptSingleLine, /*isDocBlock*/ false, new SourceRange(start.range.fromInclusive, scanner.getIndex()));
        if (parseTypes && !isInSomeContext(ParseContext.typeAnnotation)) {
            parseTypeAnnotationsFromPreParsedTrivia(comment);
        }

        return comment;
    }

    function parseScriptMultiLineComment() : Comment {
        const startToken = parseExpectedTerminal(TokenType.FORWARD_SLASH_STAR, ParseOptions.noTrivia);
        const isDocBlock = peekChar() === "*";
        scanToNextToken([TokenType.STAR_FORWARD_SLASH]);
        const endToken = parseExpectedTerminal(TokenType.STAR_FORWARD_SLASH, ParseOptions.noTrivia, "Unterminated multiline script comment.");
        const comment = Comment(CommentType.scriptMultiLine, isDocBlock, new SourceRange(startToken.range.fromInclusive, endToken.range.toExclusive));

        if (isDocBlock) {
            parseDocBlockFromPreParsedComment(comment);
        }
        else if (parseTypes && !isInSomeContext(ParseContext.typeAnnotation)) {
            parseTypeAnnotationsFromPreParsedTrivia(comment);
        }

        return comment;
    }

    /**
     * parse triva - comments and whitespace
     * what type of comments are parsed depends on current scanner mode (tag | script)
     * we need to consider the case where we are parsing types, do we parse both tag and script comments?
     */
    function parseTrivia() : Node[] {
        const result : Node[] = [];

        const savedContext = updateParseContext(ParseContext.trivia);

        while (true) {
            switch (lookahead()) {
                case TokenType.CF_TAG_COMMENT_START: {
                    if (!tagMode()) break;
                    result.push(parseTagComment());
                    continue;
                }
                case TokenType.DBL_FORWARD_SLASH: {
                    if (!scriptMode()) break;
                    const comment = parseScriptSingleLineComment();
                    result.push(comment);
                    continue;
                }
                case TokenType.FORWARD_SLASH_STAR: {
                    if (!scriptMode()) break;
                    const comment = parseScriptMultiLineComment();
                    result.push(comment);
                    continue;
                }
                case TokenType.WHITESPACE: {
                    if (debug) {
                        const nextToken = next();
                        result.push(TextSpan(nextToken.range, scanner.getTextSlice(nextToken.range)));
                    }
                    else {
                        result.push(TextSpan(next().range, ""));
                    }
                    continue;
                }
            }
            // if we didn't match tag comment start or whitespace, we're done
            break;
        }

        let finalizedResult : Node[];
        if (isInSomeContext(ParseContext.hashWrappedExpr) || isInSomeContext(ParseContext.insideCfTagAngles)) {
            finalizedResult = (result as CfTag[]).map((v) => {
                if (v.tagType === CfTag.TagType.comment) {
                    return Comment(v);
                }
                else {
                    if (debug) {
                        return TextSpan(v.range, scanner.getTextSlice(v.range));
                    }
                    else {
                        return TextSpan(v.range, "");
                    }
                }
            })
        }
        else {
            finalizedResult = result;
        }

        parseContext = savedContext; // clear trivia context; parsing types inside comments is not considered trivia

        return finalizedResult;
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
                    const savedContext = parseContext;
                    // @fixme - just return the <cfscript> tag, and then eat the script body from the caller;
                    // then we don't need to worry about pop/restore of this context here
                    parseContext &= ~(1 << ParseContext.insideCfTagAngles);
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
                    parseContext = savedContext;
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

    function parseTagAttributes(maxToParse = -1) : TagAttribute[] {
        const parseAll = maxToParse === -1;
        const result : TagAttribute[] = [];

        let attrNameLexeme;
        while ((parseAll || result.length < maxToParse) && (attrNameLexeme = scanTagAttributeName())) {
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

                result.push(TagAttribute(attrName, attrName.token.text, equal, value));
            }
            else {
                result.push(TagAttribute(attrName, attrName.token.text));
            }
        }

        return result;
    }

    const enum ComponentSyntaxType { tag = 1, script = 2 }; // start at non-zero
    const enum ComponentOrInterface { component = 1, interface = 2 };

    interface ComponentPreamble {
        primarySyntax: ComponentSyntaxType,
        type: ComponentOrInterface,
        preamble: Node[],
    }

    function parseComponentPreamble() : ComponentPreamble | null {
        setScannerMode(ScannerMode.tag | ScannerMode.script);
        const preamble : Node[] = []; // not certain, but probably just {Comment | ImportStatement}

        //
        // speculationhelper will cleanup scannermode changes
        //
        function isTagComponentOrInterfaceBlock() {
            setScannerMode(ScannerMode.tag);
            if (lookahead() !== TokenType.CF_START_TAG_START) {
                return false;
            }
            const tag = parseCfStartTag();
            return tag.canonicalName === "component" 
                ? ComponentOrInterface.component
                : tag.canonicalName === "interface"
                ? ComponentOrInterface.interface
                : null;
        }

        function isScriptComponentOrInterfaceBlock() {
            setScannerMode(ScannerMode.script);
            const peekedText = peek().text.toLowerCase();
            const cOrI = peekedText === "component"
                ? ComponentOrInterface.component
                : peekedText === "interface"
                ? ComponentOrInterface.interface
                : null;

            if (!cOrI) {
                return null;
            }

            next();
            parseTrivia();
            parseTagAttributes();
            return peek().type === TokenType.LEFT_BRACE ? cOrI : null;
        }

        let gotNonComment = false;
        let gotTagComment = false;
        let gotScriptComment = false;
        let syntaxType : ComponentSyntaxType | null = null;
        let componentOrInterface : ComponentOrInterface | null = null;

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
                    const comment = parseScriptMultiLineComment();
                    preamble.push(comment);
                    gotScriptComment = true;
                    continue;
                }
                case TokenType.LEXEME: {
                    const maybeCOrI = SpeculationHelper.lookahead(isScriptComponentOrInterfaceBlock);
                    if (maybeCOrI) {
                        syntaxType = ComponentSyntaxType.script;
                        componentOrInterface = maybeCOrI;
                        break outer;
                    }
                    gotNonComment = true;
                    next();
                    continue;
                }
                case TokenType.CF_START_TAG_START: {
                    const maybeCOrI = SpeculationHelper.lookahead(isTagComponentOrInterfaceBlock);
                    if (maybeCOrI) {
                        syntaxType = ComponentSyntaxType.tag;
                        componentOrInterface = maybeCOrI;
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
            // should be "only comments or import statements"
            //parseErrorAtRange(0, getIndex(), "A component preamble may only contain comments.");
        }

        if (syntaxType && ((syntaxType === ComponentSyntaxType.tag && gotScriptComment) || (syntaxType === ComponentSyntaxType.script && gotTagComment))) {
            // we also need to consider "import" statements
            //parseErrorAtRange(0, getIndex(), `A ${match} component preamble may only contain ${match}-style comments.`);
        }
        else if (!syntaxType) {
            parseErrorAtRange(0, getIndex(), `A CFC file must contain a component or interface definition.`);
        }

        if (syntaxType && componentOrInterface) {
            return {
                primarySyntax: syntaxType,
                type: componentOrInterface,
                preamble,
            };
        }
        else return null;
    }

    function acquire(sourceFile_: SourceFile) {
        sourceFile = sourceFile_;
        typedefContainer = sourceFile;
        scanner = sourceFile.scanner;
        parseContext = ParseContext.none;
        diagnostics = sourceFile.diagnostics = [];
    }

    function release() : void {
        (scanner as any) = undefined;
        (sourceFile as any) = undefined;
        (mode as any) = undefined;
        parseContext = ParseContext.none;
        (lookahead_ as any)  = undefined;
        (token_ as any) = undefined;
        peek0Cache_ = null;
        (lastNonTriviaToken as any) = undefined;
        diagnostics = [];
        (typedefContainer as any) = undefined;
        lastDocBlock = null;
        parseErrorMsg = null;
    }

    // fixme: we can supply a filetype, but have to set the sourceFile with an earlier call?
    function parse(sourceFile_: SourceFile) : void {
        acquire(sourceFile_);

        const cfFileType = sourceFile.cfFileType || CfFileType.cfm

        switch (cfFileType) {
            case CfFileType.cfm:
                sourceFile.content = parseTags();
                break;
            case CfFileType.cfc: {
                const componentInfo = parseComponentPreamble();
                if (!componentInfo) {
                    sourceFile.content = [];
                    break;
                }

                if (componentInfo.type === ComponentOrInterface.interface) {
                    // fixme:
                    // we don't parse interface files yet, mostly because we don't parse function declarations
                    // we also don't later check that a cfc correctly implements the interfaces it says it implements, either
                    return;
                    //updateParseContext(ParseContext.interface);
                }
                else {
                    //
                    // when parsing a component (tag or script), update the context to say we are in the top-level of a component
                    // the only way to get out of this context from within a component is to descend into a function definition
                    //
                    // <cfcomponent>
                    //     <!--- psuedoconstructor context --->
                    //     <cfscript>
                    //        /* also psuedoconstructor context */
                    //        function foo() { /* no longer psuedoconstructor */ }
                    //     </cfscript>
                    // </cfcomponent>
                    //
                    updateParseContext(ParseContext.cfcPsuedoConstructor);
                }

                sourceFile.content.push(...componentInfo.preamble);

                // extract parsed types into working node's typedefs
                // should probably break this out so we can use it on any current working node
                // also this more of the binder's responsibility, it would do a better job of checking for duplicate declarations and etc.
                if (lastDocBlock) {
                    for (const typeshim of lastDocBlock.typedefs) {
                        switch (typeshim.shimKind) {
                            case TypeShimKind.interfacedef: {
                                if (sourceFile.containedScope.typeinfo.interfaces.has((typeshim.type as Interface).name)) {
                                    sourceFile.containedScope.typeinfo.interfaces.get((typeshim.type as Interface).name)!.push(typeshim.type as Interface);
                                }
                                else {
                                    sourceFile.containedScope.typeinfo.interfaces.set((typeshim.type as Interface).name, [typeshim.type as Interface]);
                                }
                                break;
                            }
                            case TypeShimKind.typedef: {
                                sourceFile.containedScope.typeinfo.aliases.set(typeshim.name, typeshim.type);
                                break;
                            }
                            case TypeShimKind.namespace: {
                                sourceFile.containedScope.typeinfo.namespaces.set(typeshim.name, typeshim);
                                break;
                            }
                            case TypeShimKind.import: {
                                sourceFile.containedScope.typeinfo.imports.set(typeshim.qualifiedName, typeshim);
                                break;
                            }
                            default: exhaustiveCaseGuard(typeshim);
                        }
                        // @decorator -- not supporting decorators
                        // else if (typeshim.what === "decorator") {
                        //     sourceFile.containedScope.typedefs.decorators.push(typeshim.type as Decorator);
                        // }
                    }
                }

                if (componentInfo.primarySyntax === ComponentSyntaxType.tag) {
                    sourceFile.content.push(...parseTags());
                }
                else {
                    sourceFile.content.push(...parseScript());
                }

                break;
            }
            case CfFileType.dCfm: {
                const typedefs = parseTypeAnnotations().typedefs;
                typedefContainer = sourceFile;
                maybePushTypeinfoIntoToCurrentTypeinfoContainer(typedefs);
                break;
            }
        }

        release();
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

            // @ ts-expect-error - unused
            function next() : never { // shadow the outer next()
                throw "use nextTag";
            }
            next;
            // @ ts-expect-error - unused
            function peek() : never { // shadow the outer peek()
                throw "use peekTag";
            }
            peek;
            
            function nextTag() {
                return tagList[tagIndex++];
            }
            function peekTag() {
                return hasNextTag() ? tagList[tagIndex] : null;
            }

            function parseOptionalTag(which: CfTag.Which, canonicalName: string) : CfTag | null {
                if (!hasNextTag()) return null;
                const tag = peekTag()!;
                if (tag.kind === NodeKind.hashWrappedExpr) return null;

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
                    if (lastTag.kind === NodeKind.tag && lastTag.tagType === CfTag.TagType.script) {
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
                    if (node.kind === NodeKind.textSpan || node.kind === NodeKind.comment) {
                        getTriviaOwner().push(node);
                        i++;
                        continue;
                    }
                    if (node.kind === NodeKind.tag && node.canonicalName === "argument") {
                        params.push(Tag.FunctionParameter(node as CfTag.Common));
                        i++;
                        continue;
                    }
                    break;
                }

                body = body.splice(i);
                for (const node of body) {
                    if (node.kind === NodeKind.tag && node.which === CfTag.Which.start && node.canonicalName === "argument") {
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
                    if ((node.kind === NodeKind.catch)) {
                        gotCatch = true;
                        break;
                    }
                    else if (node.kind === NodeKind.finally) {
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
                        if (debug) throw "no trivia owner?";
                        else return [];
                    }
                }

                if (gotCatch) {
                    while (index < body.length) {
                        const node = body[index];
                        if (node.kind === NodeKind.finally) {
                            gotFinally = true;
                            break;
                        }

                        if (node.kind === NodeKind.catch) {
                            catchBlocks.push(node as Tag.Catch);
                        }
                        else if (node.kind !== NodeKind.textSpan && node.kind !== NodeKind.comment) {
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
                        if (node.kind !== NodeKind.textSpan && node.kind !== NodeKind.comment) {
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
                    if (node.kind === NodeKind.switchCase) {
                        cases.push(node as Tag.SwitchCase);
                        continue;
                    }
                    else if (node.kind !== NodeKind.textSpan && node.kind !== NodeKind.comment) {
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
                    if (node.kind === NodeKind.tag) {
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
                        if (result[i].kind === NodeKind.tag) {
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
                    if (tag.kind === NodeKind.hashWrappedExpr) {
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
                            case "property": {
                                result.push(Tag.Property(tag as CfTag.Common));
                                nextTag();
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
                    const savedLastDocBlock = lastDocBlock;
                    const tag = doInExtendedContext(ParseContext.insideCfTagAngles, parseCfStartTag);
                    tagContext.update(tag);

                    if (sourceFile.cfFileType === CfFileType.cfc && isInSomeContext(ParseContext.cfcPsuedoConstructor) && tagContext.inFunction()) {
                        dropParseContext(ParseContext.cfcPsuedoConstructor);
                    }

                    if (savedLastDocBlock) {
                        tag.typeAnnotation = savedLastDocBlock.typeAnnotation;
                    }

                    result.push(tag);
                    lastDocBlock = null;
                    continue;
                }
                case TokenType.CF_END_TAG_START: {
                    const tag = doInExtendedContext(ParseContext.insideCfTagAngles, parseCfEndTag);
                    tagContext.update(tag);

                    if (sourceFile.cfFileType === CfFileType.cfc && !isInSomeContext(ParseContext.cfcPsuedoConstructor) && !tagContext.inFunction()) { // we're back in the psuedo-constructor
                        updateParseContext(ParseContext.cfcPsuedoConstructor);
                    }

                    result.push(tag);
                    lastDocBlock = null;
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

    function parseStringBody(quoteDelimiter: TokenType.QUOTE_SINGLE | TokenType.QUOTE_DOUBLE, stringUniverse: StringUniverse) : (TextSpan | HashWrappedExpr)[] {
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
                        switch (stringUniverse) {
                            case StringUniverse.value: {
                                finishTextRange();
                                result.push(parseHashWrappedExpression());
                                continue;
                            }
                            case StringUniverse.type: {
                                finishTextRange();
                                const leftHash = parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);
                                const type_fixme_shimmedAsAnnotation = TypeAnnotation(parseTypeIdWithMaybeExplicitInfer());
                                const rightHash = parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);
                                result.push(HashWrappedExpr(leftHash, type_fixme_shimmedAsAnnotation, rightHash));
                                continue;
                            }
                            default: exhaustiveCaseGuard(stringUniverse);
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
            case NodeKind.indexedAccess:
            case NodeKind.identifier:
            case NodeKind.simpleStringLiteral:          // <cfset "x" = "y">
            case NodeKind.interpolatedStringLiteral:    // <cfset "#x#" = 42> <!--- same as <cfset y = 42>
                return true;
            case NodeKind.hashWrappedExpr:
                return isAssignmentTarget(node.expr);
            default:
                return false;
        }
    }

    function parseAssignmentOrLower() : Node {
        const savedLastDocBlock = lastDocBlock;
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
        const root = parseAnonymousFunctionDefinitionOrExpression();

        if (!isAssignmentOperator()) {
            // if we're in a `for` context, we just got the following:
            // for (var x.y.z 
            //      ^^^^^^^^^
            // so we hope to see an `in` following this; otherwise, it *also* needs an initializer
            // but we can flag that in the antecedent `for` parser
            if (!isInSomeContext(ParseContext.for) && varModifier && engineVersion.engine === Engine.Adobe) {
                parseErrorAtRange(root.range, "Variable declarations require initializers.");
            }

            if (root.kind === NodeKind.identifier
                || root.kind === NodeKind.indexedAccess
                && root.accessElements.every(e => e.accessType === IndexedAccessType.dot || e.accessType === IndexedAccessType.bracket)) {
                // @todo - if the access type is not homogenous cf will error, at least at the top-most scope
                // a["b"]["c"] = 0 declares and inits a = {b: {c: 0}};
                // a["b"].c = 0 is an error ("a" is not defined)
                
                //const identifier = root;//root.kind === NodeType.identifier ? root : Identifier(root, getTriviallyComputableString(root));
                if (isInSomeContext(ParseContext.for)) {
                    return VariableDeclaration(finalModifier, varModifier, root);
                }
            }

            if (!isInSomeContext(ParseContext.for) && varModifier && root.kind === NodeKind.identifier) {
                // we have something like `var foo;`
                // if there is a final modifier we should mark it as an error
                // although, lucee does not support final but supports declarations
                // whereas ACF supports final but does not support declarations
                return VariableDeclaration(finalModifier, varModifier, root);
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
            if (savedLastDocBlock) {
                declaration.typeAnnotation = coalesceTypeAnnotationsToBindToNode(savedLastDocBlock);
                
                //
                // clear lastTypeAnnotation *if* it hasn't changed since we entered this method
                // it may have been updated to a new annotation during descent;
                // e.g.
                //
                // // @type {ok: boolean}
                // var x = {ok: true} <--- no semicolon
                // // @type number <--- gets consumed as trivia while parsing {ok: true}, this is now the global lastTypeAnnotation
                // var y = 42
                //
                if (lastDocBlock === savedLastDocBlock) lastDocBlock = null;
            }
            return declaration;
        }
        else {
            if (savedLastDocBlock) {
                assignmentExpr.typeAnnotation = savedLastDocBlock.typeAnnotation;
                if (lastDocBlock === savedLastDocBlock) lastDocBlock = null;
            }
            return assignmentExpr;
        }
    }

    function parseAnonymousFunctionDefinitionOrExpression() : Node {
        if (lookahead() === TokenType.KW_FUNCTION) {
            return parseAnonymousFunctionDefinition();
        }

        return tryParseArrowFunctionDefinition() ?? parseExpression();
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
                case TokenType.BACK_SLASH: // quotient operator, `4.2 \ 2 == 2`
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
        const arrowFunction = tryParseArrowFunctionDefinition();
        if (arrowFunction) return arrowFunction;

        switch (lookahead()) {
            case TokenType.LEFT_PAREN: {

                const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                const expr = engineVersion.engine === Engine.Adobe
                    ? parseAnonymousFunctionDefinitionOrExpression()
                    : parseAssignmentOrLower()
                const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                let root : Node = Parenthetical(leftParen, expr, rightParen);

                //
                // this message could be better especially at a use site like a concise arrow function:
                // `() => ({x:1})` is illegal, but the message could be "Consider an explicit return"
                // anyway, at least its flagged
                //
                if (engineVersion.engine === Engine.Adobe) {
                    switch (stripOuterParens(root).kind) {
                        case NodeKind.arrayLiteral:
                            parseErrorAtRange(root.range, "Parenthesized array literals are illegal. Consider removing the parentheses.");
                            break;
                        case NodeKind.structLiteral:
                            parseErrorAtRange(root.range, "Parenthesized struct literals are illegal. Consider removing the parentheses.");
                            break;
                    }
                }

                root = parseCallExpressionOrLowerRest(root);

                // (function() {})()      valid cf2021+
                // (() => value)()        valid cf2021+
                // (function() {}).v()    invalid in acf
                // ([1,2,3])[1]           invalid in acf
                // ({x:1}).x              invalid in acf
                // ({x:1})["x"]           invalid in acf
                //
                // all of the above are valid in lucee
                //
                if (engineVersion.engine === Engine.Adobe && (
                    (root.kind === NodeKind.indexedAccess)
                    || (root.kind === NodeKind.unaryOperator && root.expr.kind === NodeKind.indexedAccess)
                    || (root.kind === NodeKind.callExpression && root.left.kind === NodeKind.indexedAccess))) {
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

        const identifier = parseIdentifier();
        return parseStaticAccessOrCallExpressionOrLowerRest(identifier);
    }

    /**
     * we have identifier "foo"
     * what follows may be a callExpressionOrLower chain (e.g. foo.bar[3].baz()++)
     * or it may be a qualified pathname followed by a static access token followed by a callexpression or lower (e.g. `foo.bar.baz::indexed["access"]().chain`)
     * it seems like static access is only allowable at the "top level", because you can't do something like `#pathnameVar#::member`, so you can't do `x.y.getPathName()::foo`
     */
    function parseStaticAccessOrCallExpressionOrLowerRest(root: Identifier) : Node {
        let workingRoot : Identifier | IndexedAccess<Identifier> = root;
        while (lookahead() === TokenType.DOT) {
            const dot = parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
            const propertyName = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ true);

            workingRoot = transformingPushAccessElement(workingRoot, DotAccess(dot, propertyName));
            continue;
        }

        // we've parsed "id ['.' id]*"
        // if what follows is '::', convert what we've got so far into a string, use it as the pathname and the right-hand-side is a callExpressionOrLower
        if (lookahead() === TokenType.DBL_COLON) {
            const headTerminal = workingRoot.kind === NodeKind.identifier ? workingRoot.source : workingRoot.root.source;
            const qualifiedID  = DottedPath(headTerminal)
            if (workingRoot.kind === NodeKind.indexedAccess) {
                for (const element of (workingRoot.accessElements as DotAccess[])) {
                    pushDottedPathElement(qualifiedID, element.dot, element.property);
                }
            }

            const dblColon     = parseExpectedTerminal(TokenType.DBL_COLON, ParseOptions.withTrivia);
            const rhsHead      = parseIdentifier();
            const rhs          = parseCallAndIndexedAccessChain(rhsHead);
            const staticAccess = StaticAccess(qualifiedID, dblColon, rhs);
            const withPostfix  = parsePostfixUnaryOperator(staticAccess);
            return withPostfix;
        }
        // what follows is not a static access, but is the end of the initial sequence of dot-separated names
        // this is not a static access, we can use what we've parsed so far and continue parsing callExpressionOrLowerRest
        else {
            const indexedAccessChain = parseCallAndIndexedAccessChain(workingRoot);
            const withPostfix = parsePostfixUnaryOperator(indexedAccessChain);
            return withPostfix;
        }
    }

    /**
     * given some root, parse a chain of dot/bracket | call expression accesses, and a postfix ++/-- operator
     * something like `a.b["c"]().d["e"]++`
     */
    function parseCallExpressionOrLowerRest<T extends Node>(root: T) : Node {
        const chain = parseCallAndIndexedAccessChain(root);
        const chainWithMaybePostfixUnaryOperator = parsePostfixUnaryOperator(chain);
        return chainWithMaybePostfixUnaryOperator;
    }

    function parsePostfixUnaryOperator(postfixable: Node) : Node {
        switch (lookahead()) {
            case TokenType.DBL_PLUS:
            case TokenType.DBL_MINUS:
                const unaryOp = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                return UnaryOperator(postfixable, unaryOp);
        }
        return postfixable;
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
    function transformingPushAccessElement<T extends Node>(root: T, accessElement: IndexedAccessChainElement) : T extends IndexedAccess<infer U> ? IndexedAccess<U> : IndexedAccess<T> {
        const v : IndexedAccess = root.kind === NodeKind.indexedAccess ? root : IndexedAccess(root);
        pushAccessElement(v, accessElement);
        return v as any;
    }

    function parseCallAndIndexedAccessChain<T extends Node>(root: T) : Node {
        let workingRoot : T | IndexedAccess | CallExpression = root;
        outer:
        while (true) {
            switch (lookahead()) {
                case TokenType.LEFT_PAREN: {
                    workingRoot = parseCallExpression(workingRoot) as T;
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
                            workingRoot                 = transformingPushAccessElement(workingRoot, optionalBracketAccess);
                            parseErrorAtRange(questionMark.range.fromInclusive, rightBracket.range.toExclusive, "CF does not support optional bracket access expressions.");
                            continue;
                        }
                        else if (lookahead() === TokenType.LEFT_PAREN) {
                            workingRoot = transformingPushAccessElement(workingRoot, OptionalCall(questionMark, dot)) // as T;
                            workingRoot = parseCallExpression(workingRoot) as T;
                            parseErrorAtRange(questionMark.range.fromInclusive, (<CallExpression>workingRoot).rightParen.range.toExclusive, "CF does not support optional call expressions.");
                            continue;
                        }
                        else {
                            const propertyName = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ true, "Expected a property name.");
                            workingRoot        = transformingPushAccessElement(workingRoot, OptionalDotAccess(questionMark, dot, propertyName)) // as T;
                            // todo -- parseError if first char of propertyName is an ascii digit?
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

                    workingRoot = transformingPushAccessElement(workingRoot, BracketAccess(leftBracket, expr, rightBracket)) // as T;
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

                    workingRoot = transformingPushAccessElement(workingRoot, DotAccess(dot, propertyName)) // as T;
                    continue;
                }
            }
            break;
        }

        return workingRoot;

        function parseArrayIndexOrSliceExpression() {
            // with the introduction of static access lookup "::", a double colon is now a token
            // so an 'empty slice expression' can now be 3 tokens `[::]`, which previously was 4
            if (lookahead() === TokenType.DBL_COLON) {
                const dblColon = parseExpectedTerminal(TokenType.DBL_COLON, ParseOptions.withTrivia);
                // "reparse" the double colon as a single colon
                // any trivia is attached to the second colon
                const colon1 = Terminal(Token(TokenType.COLON, ":", new SourceRange(dblColon.range.fromInclusive, dblColon.range.fromInclusive+1)));
                const colon2 = Terminal(Token(TokenType.COLON, ":", new SourceRange(dblColon.range.fromInclusive + 1, dblColon.range.toExclusive)), dblColon.trivia);
                // we may have `x[::expr]`
                const stride = isStartOfExpression() ? parseExpression() : null;
                return SliceExpression(null, colon1, null, colon2, stride);
            }

            const first = isStartOfExpression()
                ? parseExpression()
                : lookahead() === TokenType.COLON
                ? parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia)
                : (parseErrorAtCurrentToken("Expression expected."), createMissingNode(Identifier(NilTerminal(pos()), "")));
                
            if (first.kind !== NodeKind.terminal && lookahead() === TokenType.DBL_COLON) {
                const dblColon = parseExpectedTerminal(TokenType.DBL_COLON, ParseOptions.withTrivia);
                // "reparse" the double colon as a single colon
                // any trivia is attached to the second colon
                const colon1 = Terminal(Token(TokenType.COLON, ":", new SourceRange(dblColon.range.fromInclusive, dblColon.range.fromInclusive+1)));
                const colon2 = Terminal(Token(TokenType.COLON, ":", new SourceRange(dblColon.range.fromInclusive + 1, dblColon.range.toExclusive)), dblColon.trivia);
                // we may have `x[(first)::expr]`
                const stride = isStartOfExpression() ? parseExpression() : null;
                return SliceExpression(first, colon1, null, colon2, stride);
            }

            // we got an expression and the next token is not a colon -- so this is just a basic index expression
            // like `x[e]`
            if (first.kind !== NodeKind.terminal && lookahead() !== TokenType.COLON) {
                return first;
            }


            // otherwise, we got a slice expression
            let from : Node | null = first.kind === NodeKind.terminal ? null : first;
            let colon1: Terminal = first.kind === NodeKind.terminal ? first : parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
            let to : Node | null = isStartOfExpression() ? parseExpression() : null;
            let colon2: Terminal = parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
            let stride : Node | null = isStartOfExpression() ? parseExpression() : null;
            return SliceExpression(from, colon1, to, colon2, stride);
        }

        function previousElement() : T | IndexedAccessChainElement | CallExpression  {
            if (workingRoot.kind !== NodeKind.indexedAccess) {
                return workingRoot;
            }
            else {
                const element = (workingRoot as IndexedAccess).accessElements[(workingRoot as IndexedAccess).accessElements.length-1];
                return element;
            }
        }

        function previousElementIsIdentifier() {
            if (workingRoot.kind !== NodeKind.indexedAccess) {
                return workingRoot.kind === NodeKind.identifier;
            }

            // if root is an IndexedAccess node, it is because there is at least one contained access element
            const element = (workingRoot as IndexedAccess).accessElements[(workingRoot as IndexedAccess).accessElements.length-1];
            return element.accessType === IndexedAccessType.dot
        }
    }

    function parseNewExpression() {
        const newToken       = parseExpectedTerminal(TokenType.KW_NEW, ParseOptions.withTrivia);
        const className      = lookahead() === TokenType.QUOTE_SINGLE || lookahead() === TokenType.QUOTE_DOUBLE
            ? parseStringLiteral()
            : parseDottedPath();
        const callExpression = parseCallExpression(className);
        const newExpression  = New(newToken, callExpression);
        return parseCallExpressionOrLowerRest(newExpression);
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
            case TokenType.DOT:
                return true;
            default:
                return isIdentifier();
        }
    }

    function isValidTypeIdOrBuiltinTypename(tokenType: TokenType) : boolean {
        // things like NOT and FUNCTION can be type names
        return tokenType === TokenType.LEXEME || (TokenType._FIRST_KW < tokenType && tokenType < TokenType._LAST_KW)
            || (TokenType._FIRST_LIT < tokenType
                && tokenType < TokenType._LAST_LIT
                // "multi-token" tokens can't be used -- `type T = {does not contain: 42}` is not a valid type
                && tokenType !== TokenType.LIT_DOES_NOT_CONTAIN
                && tokenType !== TokenType.LIT_IS_NOT);
    }

    function isStartOfType() : boolean {
        switch (lookahead()) {
            case TokenType.PIPE:
            case TokenType.LEFT_BRACKET:
            case TokenType.LEFT_PAREN:
            case TokenType.LEFT_BRACE:
            case TokenType.NUMBER:
            case TokenType.QUOTE_DOUBLE:
            case TokenType.QUOTE_SINGLE:
            case TokenType.LEFT_ANGLE:
            case TokenType.LEXEME:
                return true;
            default:
                return isValidTypeIdOrBuiltinTypename(lookahead());
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
        if (exprOrArgName.kind === NodeKind.identifier || isSimpleOrInterpolatedStringLiteral(exprOrArgName)) {
            let equalOrComma : Terminal | null;
            if (equalOrComma =
                    (parseOptionalTerminal(TokenType.EQUAL, ParseOptions.withTrivia) ??
                    (parseOptionalTerminal(TokenType.COLON, ParseOptions.withTrivia)))) {
                const name = exprOrArgName;
                let dotDotDot : Terminal | null = null;
                let expr : Node;

                if (engineVersion.engine === Engine.Adobe && isSimpleOrInterpolatedStringLiteral(name)) {
                    parseErrorAtRange(name.range, "String literals cannot be used as argument names.");
                }

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
            case ParseContext.typeNamespace:
                // in a namespace, we can parse if it starts with "@!"
                // how we deal with comments in this context needs work, especially
                // since we are probably already in a comment to declare the namespace,
                // because `/* @!namespace Foo { ... } */` is intended to be valid
                return peek().text === "@" && peek(1).text === "!";
            case ParseContext.arrayLiteralBody:
            case ParseContext.structLiteralBody:
            case ParseContext.argOrParamList:
                return isStartOfExpression();
            case ParseContext.cfScriptTagBody:
            case ParseContext.switchClause:
            case ParseContext.blockStatements:
                return isStartOfStatement();
            case ParseContext.typeStruct:
                if (lookahead() === TokenType.DOT_DOT_DOT) return true;
                // else fallthrough
            case ParseContext.typeTupleOrArrayElement:
                return isStartOfType();
            case ParseContext.typeParamList:
                return isValidTypeIdOrBuiltinTypename(lookahead());
            default:
                return false;
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
            case ParseContext.typeNamespace:
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
        if (engineVersion.engine === Engine.Lucee) {
            return parseExpression();
        }

        // fixme: consider dotted path
        const maybeLexemeLikeKey = scanLexemeLikeStructKey();
        if (maybeLexemeLikeKey) {
            return Identifier(
                Terminal(maybeLexemeLikeKey, parseTrivia()),
                maybeLexemeLikeKey.text);
        }

        let result = parseExpression();
        switch (result.kind) {
            case NodeKind.hashWrappedExpr:
            case NodeKind.simpleStringLiteral:
            case NodeKind.interpolatedStringLiteral:
                return result;
            default: {
                parseErrorAtRange(result.range, "Invalid struct initializer key.");
                return result;
            }
        }
    }

    function parseStructLiteralInitializerMember() : StructLiteralInitializerMember {
        if (lookahead() === TokenType.DOT_DOT_DOT) {
            const dotdotdot = parseExpectedTerminal(TokenType.DOT_DOT_DOT, ParseOptions.withTrivia);
            const expr = parseExpression();
            const maybeComma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            return SpreadStructLiteralInitializerMember(dotdotdot, expr, maybeComma);
        }
        else {
            const key = parseStructLiteralInitializerKey();
            if (lookahead() === TokenType.COMMA || lookahead() === TokenType.RIGHT_BRACE) {
                return ShorthandStructLiteralInitializerMember(key, parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia));
            }
            const colonOrEqual = lookahead() === TokenType.EQUAL
                ? parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia)
                : parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
            const value = parseAnonymousFunctionDefinitionOrExpression();
            const maybeComma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            return KeyedStructLiteralInitializerMember(key, colonOrEqual, value, maybeComma);
        }
    }

    function parseStructLiteral() : Node {
        const leftBrace = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
        const kvPairs = parseList(ParseContext.structLiteralBody, parseStructLiteralInitializerMember);
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

        function isExpressionThenColonOrEquals() {
            parseExpression();
            return lookahead() === TokenType.COLON || lookahead() === TokenType.EQUAL;
        }

        if (SpeculationHelper.lookahead(isExpressionThenColonOrEquals)) {
            const useArrayBodyListTerminator = ParseContext.arrayLiteralBody;
            const structMembers = parseList(useArrayBodyListTerminator, parseStructLiteralInitializerMember);
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
                    if (maybeComma && engineVersion.engine === Engine.Adobe && lookahead() === TokenType.RIGHT_BRACKET) {
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
                    if (maybeComma && engineVersion.engine === Engine.Adobe && lookahead() === TokenType.RIGHT_BRACKET) {
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

    // withTrivia is to support instances where the caller may want to manually consume trivia,
    // in order to grab type annotations that are inside comments
    function parseIdentifier(withTrivia = true) : Identifier {
        let terminal : Terminal;
        let name : string;

        if (!isIdentifier()) {
            parseErrorAtPos(lastNonTriviaToken?.range.toExclusive ?? 0, parseErrorMsg ?? "Expected an identifier.");

            terminal = NilTerminal(pos());
            name = "";
        }
        else {
            terminal = Terminal(scanIdentifier()!, withTrivia ? parseTrivia() : []);
            name = terminal.token.text;
        }

        // @fixme: need to mark node with error flags if there was an error
        return Identifier(terminal, name);
    }

    function parseStringLiteral(stringUniverse = StringUniverse.value) : SimpleStringLiteral | InterpolatedStringLiteral {
        const quoteType = lookahead();
        if (quoteType !== TokenType.QUOTE_SINGLE && quoteType !== TokenType.QUOTE_DOUBLE) {
            // will a lookahead or speculate ever trigger this ... ?
            throw "AssertionFailure: parseStringLiteral called on input without valid string delimiter";
        }

        const leftQuote = parseExpectedTerminal(quoteType, ParseOptions.noTrivia);
        const stringElements = parseStringBody(quoteType, stringUniverse);
        const rightQuote = parseExpectedTerminal(quoteType, ParseOptions.withTrivia);

        if (stringElements.length === 0) {
            const emptyTextSpan = TextSpan(new SourceRange(leftQuote.range.toExclusive, leftQuote.range.toExclusive), "");
            return SimpleStringLiteral(leftQuote, emptyTextSpan, rightQuote);
        }
        else if (stringElements.length === 1) {
            const onlyElement = stringElements[0];
            if (onlyElement.kind === NodeKind.textSpan) {
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
    function parseDottedPath(allowTrailingGlob = false) : DottedPath {
        const result = DottedPath(parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ false));
        while (lookahead() === TokenType.DOT) {
            const dot = parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
            
            if (allowTrailingGlob && lookahead() === TokenType.STAR) {
                const glob = next();
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

    // @fixme: unify the following 2 methods
    // function parseFunctionDeclarationParameters() : cfFunctionSignatureParam[] {
    //     const params = tryParseFunctionDefinitionParameters(/*speculative*/ false, /*asDeclaration*/ true);
    //     return extractScriptFunctionParams(params);
    // }

    function parseFunctionTypeParametersArrowLike() : cfFunctionSignatureParam[] {
        const params : {name: Terminal, optional: boolean, spread: boolean, type: Type, range: SourceRange}[] = [];
        while (lookahead() !== TokenType.RIGHT_PAREN && lookahead() !== TokenType.EOF) {
            let isSpread = false;
            if (lookahead() === TokenType.DOT_DOT_DOT) {
                parseExpectedTerminal(TokenType.DOT_DOT_DOT, ParseOptions.withTrivia);
                isSpread = true;
            }

            const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/true, /*allowNumeric*/false);
            const optional = !!parseOptionalTerminal(TokenType.QUESTION_MARK, ParseOptions.withTrivia);
            parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);

            let typeStart = pos(); // fixme: add ranges to types
            let type = parseType();
            let typeEnd = pos();

            if (lookahead() !== TokenType.RIGHT_PAREN) {
                parseExpectedTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            }

            params.push({spread: isSpread, name, optional, type, range: new SourceRange(typeStart, typeEnd)});
        }
        for (let i = 0; i < params.length; i++) {
            const isLast = i === params.length - 1;
            const param = params[i];
            if (param.spread) {
                if (!isLast) {
                    parseErrorAtRange(param.range, "Spread parameter must be in last parameter position.");
                    param.type = BuiltinType.any;
                }
                // probably can't be done at parse time; (...v: T) => void ----> T could be a tuple or array, we don't know here
                // if (param.type.kind !== TypeKind.tuple && !isUninstantiatedArray(param.type) && param.name.token.text.toLowerCase() !== "arguments") {
                //     parseErrorAtRange(param.range, "A spread parameter must be an array type (unless it is the only parameter, and its name is 'arguments').");
                //     param.type = BuiltinType.any;
                // }
                if (param.name.token.text.toLowerCase() === "arguments") {
                    if (params.length > 1 || !isUninstantiatedArray(param.type) || param.type.args[0] !== BuiltinType.any) {
                        parseErrorAtRange(param.range, "The well-known spread parameter 'arguments' must be the only parameter in the signature, and it must have type 'any[]'.")
                        param.type = BuiltinType.any;
                    }
                }
            }
        }
        return params.map((param) => cfFunctionSignatureParam(!param.optional, param.type, param.name.token.text, param.spread));
    }

    // function parseFunctionTypeParameters() : cfFunctionSignatureParam[] {
    //     const params = tryParseFunctionDefinitionParameters(/*speculative*/ false, /*asDeclaration*/ true);
    //     return params.map(param => cfFunctionSignatureParam(param.required, param.type ?? SyntheticType.any, param.uiName));
    // }

    // speculative overload is last in overload set so speculation-forwarder can see it
    // this might not be maintainable in the long run ? the alternative is to not overload, and
    // use definite! syntax at non-speculative call-
    // the speculative mode is to support disambiguating arrow-function calls, which often look like other valid expressions
    // until we get a full parameter definition parse followed by `) <trivia>? =>`
    function tryParseFunctionDefinitionParameters(speculative: false, _asDeclaration: boolean) : Script.FunctionParameter[];
    function tryParseFunctionDefinitionParameters(speculative: true) : Script.FunctionParameter[] | null;
    function tryParseFunctionDefinitionParameters(speculative: boolean, _asDeclaration = false) : Script.FunctionParameter[] | null {
        const savedContext = updateParseContext(ParseContext.argOrParamList);
        const result : Script.FunctionParameter[] = [];
        const paramTypeAttrName = "xtype";
        // might have to handle 'required' specially in "isIdentifier"
        // we could set a flag saying we're in a functionParameterList to help out
        while (isStartOfExpression()) {
            let requiredTerminal : Terminal | null = null;
            let javaLikeTypename : DottedPath | null = null;
            let dotDotDot : Terminal | null = null;
            let name : Identifier;
            let equal : Terminal | null = null;
            let defaultValue : Node | null = null;
            let attrs : TagAttribute[] = [];
            let comma : Terminal | null = null;
            let type : Type | null = null;

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
                    javaLikeTypename = parseDottedPath();
                    name = parseIdentifier();
                    // todo - parse type annotation
                }
                // function foo((required)? name (= defaultExpr)?) { ... }
                //                          ^^^^
                else if (isLexemeLikeToken(peek())) {
                    name = parseIdentifier(/*withTrivia*/ true);
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
            }

            // function foo((required)? ((... name) | (type(.name)* name)) ((= defaultExpr) | attrs)?) {  }
            //                                                               ^^^^^^^^^^^^^
            equal = parseOptionalTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
            if (equal) {
                defaultValue = parseExpression();
            }

            // function foo((required)? ((... name) | (type(.name)* name)) ((= defaultExpr) | attrs)?) {  }
            //                                                                                ^^^^^
            if (lookahead() === TokenType.LEXEME) {
                if (defaultValue) {
                    const errorRange = peek().range;
                    parseErrorAtRange(errorRange, "Parameter attributes cannot follow a default value expression; to combine a default value with other attributes, use an explicit 'default' attribute.")
                }
                else {
                    attrs = parseTagAttributes();
                    defaultValue = getAttributeValue(attrs, "default") ?? null;
                    for (const attr of attrs) {
                        if (attr.canonicalName === paramTypeAttrName && attr.expr?.kind === NodeKind.simpleStringLiteral) {
                            // parse the type as though it were a loose expression;
                            // the range provided to the type parser has the leading and trailing quotes removed
                            type = parseType(attr.expr.range.fromInclusive+1, attr.expr.range.toExclusive-1);
                            break;
                        }
                    }
                }
            }

            comma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

            result.push(Script.FunctionParameter(requiredTerminal, javaLikeTypename, dotDotDot, name, equal, defaultValue, attrs, comma, type));
        }

        if (engineVersion.engine === Engine.Adobe && result.length > 0 && result[result.length-1].comma) {
            parseErrorAtRange(result[result.length-1].comma!.range, "Illegal trailing comma.");
        }

        parseContext = savedContext;

        return result;
    }

    function tryParseArrowFunctionDefinition() : ArrowFunctionDefinition | null {
        // the whole thing is speculative
        return SpeculationHelper.speculate(() => {
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
                        /*attrs*/ [],
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
                const block = doOutsideOfContext(ParseContext.cfcPsuedoConstructor, () => parseBracedBlock(ScannerMode.script));
                return ArrowFunctionDefinition(leftParen, params, rightParen, fatArrow, block);
            }
            else {
                // having matched a params list with a fat arrow, we're definitely in an arrow function, 
                // but the expression may not have been written yet (e.g `(v) => $` where '$' is EOF);
                // speculative or not, we return an arrow function
                if (!isStartOfExpression()) {
                    parseErrorAtRange(fatArrow.range.toExclusive, fatArrow.range.toExclusive+1, "Expression expected.");
                    return ArrowFunctionDefinition(leftParen, params, rightParen, fatArrow, createMissingNode(NilTerminal(pos())));
                }
                else {
                    return ArrowFunctionDefinition(
                        leftParen,
                        params,
                        rightParen,
                        fatArrow,
                        doOutsideOfContext(ParseContext.cfcPsuedoConstructor, () => engineVersion.engine === Engine.Adobe
                            ? parseAnonymousFunctionDefinitionOrExpression()
                            : parseAssignmentOrLower()));
                }
            }
        });
    }

    function stripOuterParens(node: Node) {
        while (node.kind === NodeKind.parenthetical) {
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
                next();
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

    /**
     * fixme: this pattern repeats here and in the cfc preamble when parsing a cfc
     */
    function pushTypeinfoIntoTypeinfoContainer(typedefs: (Typedef|Interfacedef|Namespace|TypeImport)[], typeinfo: TypeInfo) : void {
        for (const typeshim of typedefs) {
            switch (typeshim.shimKind) {
                case TypeShimKind.interfacedef: {
                    if (typeinfo.interfaces.has((typeshim.type as Interface).name)) {
                        typeinfo.interfaces.get((typeshim.type as Interface).name)!.push(typeshim.type as Interface);
                    }
                    else {
                        typeinfo.interfaces.set((typeshim.type as Interface).name, [typeshim.type as Interface]);
                    }
                    break;
                }
                case TypeShimKind.typedef: {
                    typeinfo.aliases.set(typeshim.name, typeshim.type);
                    break;
                }
                case TypeShimKind.namespace: {
                    typeinfo.namespaces.set(typeshim.name, typeshim);
                    break;
                }
                case TypeShimKind.import: {
                    typeinfo.imports.set(typeshim.qualifiedName, typeshim);
                    break;
                }
                default: exhaustiveCaseGuard(typeshim);
            }
        }
    }

    function maybePushTypeinfoIntoToCurrentTypeinfoContainer(typeinfo: (Typedef|Interfacedef|Namespace|TypeImport)[] | undefined) : void {
        // why would the container not have a contained scope? it's the target typedef container ... ?
        if (!typeinfo || !typedefContainer.containedScope) {
            return;
        }

        // we still will have parsed them but we might discard
        const allowNamespacesAndTypeImports = typedefContainer === sourceFile
            && (sourceFile.cfFileType === CfFileType.cfc || sourceFile.cfFileType === CfFileType.dCfm);

        if (!allowNamespacesAndTypeImports) {
            pushTypeinfoIntoTypeinfoContainer(
                typeinfo.filter(v => v.shimKind !== TypeShimKind.import && v.shimKind !== TypeShimKind.namespace),
                typedefContainer.containedScope.typeinfo);
        }
        else {
            pushTypeinfoIntoTypeinfoContainer(
                typeinfo,
                typedefContainer.containedScope.typeinfo);
        }
    }

    function tryParseNamedFunctionDefinition(speculative: false, asDeclaration: boolean) : Script.FunctionDefinition;
    function tryParseNamedFunctionDefinition(speculative: true) : Script.FunctionDefinition | null;
    function tryParseNamedFunctionDefinition(speculative: boolean, asDeclaration = false) : Script.FunctionDefinition | null {
        maybePushTypeinfoIntoToCurrentTypeinfoContainer(lastDocBlock?.typedefs);

        const savedLastDocBlock = lastDocBlock;
        const savedTypedefContainer = typedefContainer;
        lastDocBlock = null;

        let accessModifier: Terminal | null = null;
        let returnType    : DottedPath | null = null;
        let functionToken : Terminal | null;
        let nameToken     : Identifier;
        let leftParen     : Terminal;
        let params        : Script.FunctionParameter[];
        let rightParen    : Terminal;
        let attrs         : TagAttribute[];
        let body          : Block;
        
        if (SpeculationHelper.lookahead(isAccessModifier)) {
            accessModifier = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ false);
        }
        if (SpeculationHelper.lookahead(isJavaLikeTypenameThenFunction)) {
            returnType = parseDottedPath();
        }

        if (speculative) {
            functionToken = parseOptionalTerminal(TokenType.KW_FUNCTION, ParseOptions.withTrivia);
            if (!functionToken) {
                lastDocBlock = savedLastDocBlock;
                typedefContainer = savedTypedefContainer;
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

            attrs = parseTagAttributes();
            body = Block(null, [], null);
            body.range = new SourceRange(pos(), pos());

            /*discarded*/ parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.noTrivia);
        }
        else {
            rightParen    = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
            attrs         = parseTagAttributes();
            body          = doOutsideOfContext(ParseContext.cfcPsuedoConstructor, () => parseBracedBlock(ScannerMode.script));
        }

        if (savedLastDocBlock?.docBlockAttrs) {
            attrs.push(...savedLastDocBlock.docBlockAttrs);
        }
        if (savedLastDocBlock?.treatAsConstructor) {
            const syntheticAttr = TagAttribute(NilTerminal(-1), "@!init");
            // this node is synthetic, and isn't in lexical order;
            // non-lexically ordered nodes must be skipped by the tree flatener, marking as doc block acheives this, perhaps cryptically
            syntheticAttr.flags |= NodeFlags.docBlock;
            attrs.push(syntheticAttr);
        }

        let returnTypeAnnotation : Type | null = null; // fixme: unecessary?
        const result = Script.FunctionDefinition(accessModifier, returnType, functionToken, nameToken, leftParen, params, rightParen, attrs, body, returnTypeAnnotation);

        result.typeAnnotation = coalesceTypeAnnotationsToBindToNode(savedLastDocBlock);
        
        // the next legit docblock might be parsed as trivia "attached" to the end of this function's final brace
        if (lastDocBlock === savedLastDocBlock) {
            lastDocBlock = null;
        }

        typedefContainer = savedTypedefContainer;

        return result;
    }

    function coalesceTypeAnnotationsToBindToNode(docBlock: typeof lastDocBlock) : TypeAnnotation | NonCompositeFunctionTypeAnnotation | null {
        if (docBlock?.argumentAnnotations) {
            return NonCompositeFunctionTypeAnnotation(docBlock.argumentAnnotations, null);
        }
        else if (docBlock?.typeAnnotation) {
            return docBlock.typeAnnotation;
        }
        else {
            return null;
        }
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
            if (!init || init.kind !== NodeKind.variableDeclaration) {
                init = createMissingNode(Identifier(NilTerminal(pos()), ""));
                parseErrorAtRange(leftParen.range.fromInclusive+1, leftParen.range.toExclusive+1, "Declaration expected.");
            }
            const inToken = Terminal(next(), parseTrivia());
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
            // an exception type can be a variable, a dotted path, a string, or an interpolated string
            // if it is an identifier or interpolated string, at runtime it should resolve to an exception "type"
            const exceptionType    = lookahead() === TokenType.QUOTE_SINGLE || lookahead() === TokenType.QUOTE_DOUBLE
                ? parseStringLiteral()
                : parseDottedPath();
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
            case ParseContext.typeTupleOrArrayElement:
            case ParseContext.typeParamList:
                parseErrorAtCurrentToken("Expected a type expression.");
                return;
            case ParseContext.typeStruct:
                parseErrorAtCurrentToken("Type-level struct member definition expected.");
                return;
            case ParseContext.argOrParamList:
                parseErrorAtPos(lastNonTriviaToken?.range.toExclusive ?? 0, "Expression expected.");
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
        
        lastDocBlock = null; // any doc block in body should have been consumed, or is loose and is discarded (how will this handle a trailing typedef in a function or component?)

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
            config.cancellationToken.throwIfCancellationRequested();

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
                    const stmt = parseAssignmentOrLower();

                    // we may want to hold onto the semicolon later, but right now we can just discard it
                    // it is valid in this position though so we need to parse it
                    if (scriptMode()) parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);

                    return stmt;
                }
                case TokenType.LEFT_BRACE: {
                    // will we *ever* parse a block in tag mode ... ?
                    // anyway, just forward current mode; which is assumed to be script mode, if we're parsing a statement
                    return parseBracedBlock(mode);
                }
                case TokenType.LEFT_BRACKET: {
                    const expr = parseExpression();
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
                    const path = parseDottedPath(/*allowTrailingGlob*/ true);
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
                    const expr = isStartOfExpression() ? parseAssignmentOrLower() : null;
                    // if we've got a return statement we should be in a script context;
                    // but a user may not be heeding that rule
                    const semi = scriptMode() ? parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia) : null;
                    return ReturnStatement(returnToken, expr, semi);
                }
                default: {
                    const peeked = peek();
                    const peekedCanonicalText = peeked.text.toLowerCase();

                    // special case for sugared `abort`
                    if (peekedCanonicalText === "abort" && !isInSomeContext(ParseContext.sugaredAbort)) {
                        const terminal = Terminal(next(), parseTrivia());
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
                            if (statement.kind === NodeKind.statement
                                && (statement.expr!.kind === NodeKind.simpleStringLiteral || statement.expr!.kind === NodeKind.interpolatedStringLiteral)) {
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
                    
                    if (isInSomeContext(ParseContext.cfcPsuedoConstructor) && peekedCanonicalText === "property") {
                        const propertyToken = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/false, /*allowNumeric*/false);
                        const attrs = parseTagAttributes();
                        return Script.Property(propertyToken, attrs);
                    }

                    //
                    // lucee supports the following param syntax:
                    // 1) param <type> <name> ("=" <default>)? <attr>*
                    // 2) param <name> ("=" <default>)? <attr>*
                    // 3?) param <attr>+ <---- this might be indistinguishable from (2), we don't recognize it
                    // see lucee's AbstrCFMLScriptTransformer.java::_paramStatement
                    //
                    // in adobe, param is just a typical sugared tag name: `param <attr>+`
                    //
                    if (peekedCanonicalText === "param" && engineVersion.engine === Engine.Lucee) {
                        // param is also a valid variable name;
                        // we don't want to create a ParamStatement for something like `param = 42`, which is an assignment to a symbol named 'param', and has nothing to do with a ParamStatement
                        const paramToken = SpeculationHelper.speculate(() => {
                            const paramToken = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/false, /*allowNumeric*/false);
                            if (isLexemeLikeToken(peek())) return paramToken;
                            else return null;
                        });
                        if (paramToken) { // ok, we got a legit param token starting a ParamStatement
                            const nextIsLiterallyNameEquals = SpeculationHelper.speculate(() => {
                                const singleAttr = parseTagAttributes(/*maxToParse*/ 1);
                                if (singleAttr.length === 1 && singleAttr[0].canonicalName === "name" && singleAttr[0].equals && singleAttr[0].expr) {
                                    return singleAttr[0];
                                }
                                return null;
                            });

                            if (nextIsLiterallyNameEquals) {
                                // we got `"param" "name" "=" ...` which is just a basic param statement
                                const attrs = [nextIsLiterallyNameEquals, ...parseTagAttributes()];
                                /*discarded*/parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);
                                return ParamStatement(paramToken, attrs);
                            }

                            const paramWithImplicitTypeAndName = SpeculationHelper.speculate(() => {
                                const typeName = parseDottedPath();
                                if (lookahead() === TokenType.EQUAL) return null; // the implicit type will not be followed by an equals
                                const name = parseDottedPath();
                                const equals = parseOptionalTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
                                const expr = equals ? parseExpression() : null;
                                const attrs = parseTagAttributes();
                                /*discarded*/parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);

                                return ParamStatementWithImplicitTypeAndName(paramToken, typeName, name, equals, expr, attrs);
                            });

                            if (paramWithImplicitTypeAndName) {
                                return paramWithImplicitTypeAndName;
                            }

                            const name = parseDottedPath();
                            const equals = parseOptionalTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
                            const expr = equals ? parseExpression() : null;
                            const attrs = parseTagAttributes();
                            /*discarded*/parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia);

                            return ParamStatementWithImplicitName(paramToken, name, equals, expr, attrs);
                        }
                    }
                    else if (peekedCanonicalText === "param" || isSugaredTagName(peekedCanonicalText)) {
                        // fixme: also create a param statement here
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
                                    return ScriptSugaredTagCallBlock(/*name*/quickPeek[0], /*attrs*/quickPeek[1], /*block*/block);
                                }
                                else {
                                    return ScriptSugaredTagCallStatement(quickPeek[0], quickPeek[1], parseExpectedTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia))
                                }
                            }
                            // otherwise we just got a terminal
                            // e.g. `component { ...` or `transaction { ...`
                            //       ^^^^^^^^^            ^^^^^^^^^^^
                            else {
                                const block = parseBracedBlock();
                                return ScriptSugaredTagCallBlock(/*name*/quickPeek as Terminal, /*attrs*/[], /*block*/block);
                            }
                        }
                    }

                    // if the name matches the "cf..." pattern, and the next non trivia is a left paren, this is a TagLikeCall(Statement|Block)
                    // cfSomeTag(attr1=foo, attr2=bar) { ... } (taglikecall block)
                    // cfSomeTag(); (taglikecall statement)
                    if (peekedCanonicalText.length > 2 && /^cf/i.test(peekedCanonicalText)) {
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

    //
    // types / docblocks
    //
    // type annotations can appear
    //   - in a comment
    //     - in a docblock            (parseDocBlock)
    //     - in a single line comment (parseTypeAnnotationsFromPreParsedTrivia)
    //     - in a multiline comment   (parseTypeAnnotationsFromPreParsedTrivia)
    //     - in a tag comment         (parseTypeAnnotationsFromPreParsedTrivia)
    //   - in a .d.cfm file           (parseTypeAnnotations)
    //
    // - We recognize a docblock during primary parsing of comments, when we hit a multiline comment where the first character is "*" (so the source text looks like "/**")
    //   when we recognize a docblock, we scan out the entire comment to the end "*/", update the scanner's range so that it can only "see" the contents of the docblock,
    //   and enter "parseDocBlockFromPreParseComment"
    // - In the doc block parser, we parse "@foo .*" as a tag attribute where the name is "foo" and the associated node is a textspan containing the subsequent text.
    //   The scanner takes care of removing "elidable prefixes", which is the portion of a new line that is just whitespace followed by "*"
    // - If the "parseTypes" flag is set, and an attribute name is a recognized type start symbol ("typedef", "decorator", "type", "interface"),
    //   we update the scanner as appropriate and descend into a type parser
    // - Parsed types get turned into TypeShims
    // - All the tag attributes / typeshims parsed out of a docblock get put onto the parser-global "lastDocBlock" object, which all parsers have access to and may
    //   inspect and peel off tag attributes and type annotations as appropriate; this allows annotations to appear in the "comment before" any given node
    // - The binder is then responsible for putting node-attached typedefs onto appropriate containers, the parser should attach a literal type annotation to a node's typeAnnotation property
    //
    // - parseType is a typical parser function, and as long as the scanner's view is updated appropriately, calling `parseType()` isn't any different than calling `parse*`
    //   i.e. it tries to parse it's goal and issues diagnostics just like any other term-level parser
    //
    // - "parseTypeAnnotations" and "parseTypeAnnotationsFromPreParsedTrivia" should probably be unified in some way, one serves as the "statement" parser for a .d.cfm file,
    //   the other to parse type annotations out of single line comments, multiline comments, but not docblock comments; they all do just about the same thing, but assume
    //   three separate "parsing contexts" (single line / multiline / tag comment, docblock comment, .d.cfm file), each of which has subtle differences that we can probably abstract over
    //
    // - docblock tag attributes need to go into the global docblock obj, so the subsequently parsed node can grab them
    // - a type annotation neds to into the global docblock obj for the same reason
    // - typedefs need to be attached to their containing trivia, so that the binder can hoist them into the appropriate container
    //   - a typedef is visible only from ancestors of the container in which it was declared
    //   - we expect most to be "global", at the library or source level, but it is possible to declare them within functions and have their visibility limited to the function

    // sets the parser-global `lastDocBlock`
    function parseDocBlockFromPreParsedComment(node: Comment) : void {
        const from = node.range.fromInclusive + 3; // start immediately after the start sequence "/**" which kicked off the docblock
        const to = node.range.toExclusive - 2; // start immediately after the start sequence "/**" which kicked off the docblock
        const savedScannerState = getScannerState();

        restoreScannerState({
            index: from,
            artificialEndLimit: to,
            mode: ScannerMode.docBlock,
        });        

        let workingAttributeUiName : Terminal = NilTerminal(pos(), "hint");
        const docBlockAttrs : TagAttribute[] = [];
        const typedefs : (Typedef | Interfacedef | Namespace | TypeImport)[] = [];
        const argumentAnnotations : NamedAnnotation[] = [];
        let typeAnnotation : TypeAnnotation | null = null;
        let treatAsConstructor = false;

        // why this instead of parse trivia? probably some interaction with accidentally parsing comments inside comments
        function eatWhitespace() {
            while (lookahead() === TokenType.WHITESPACE) { // fixme: we are in trivia context, right?
                next();
                continue;
            }
        }

        eatWhitespace();

        while (lookahead() !== TokenType.EOF) {
            const start = pos();
            const name = scanDocBlockAttrName();

            // if we didn't get a valid name, advance and try again
            if (!name) {
                next();
                scanDocBlockAttrText();
                continue;
            }

            if (name) {
                // we need a way to use "@type" in a docblock, but "@type" is already a legitimate docblock attribute name
                // right now we use "@!type" and "@!typedef" and etc, which can be used in current docblocks, but can't represent actual tag attributes
                // i.e. in `function foo() !type="{}" {}`, "!type" is not valid in that position, so /** @!type {} */ is currently valid but probably won't conflict with much real world code
                if (name[0] === "!") {
                    if (parseTypes && name === "!interface") {
                        restoreScannerState({...getScannerState(), index: getIndex() - name.length + 1}); // backup to start of `interface` "token" (without the "!")
                        const interfaceDef = parseType();
                        if (interfaceDef.kind === TypeKind.interface) {
                            typedefs.push(Interfacedef(interfaceDef));
                        }
                        continue;
                    }
                    else if (parseTypes && name === "!typedef") {
                        eatWhitespace();
                        typedefs.push(parseTypeDef());
                        continue;
                    }
                    else if (parseTypes && name === "!type") {
                        eatWhitespace();
                        typeAnnotation = TypeAnnotation(parseType());
                        continue;
                    }
                    else if (parseTypes && name === "!init") {
                        treatAsConstructor = true;
                        continue;
                    }
                    else if (parseTypes && name === "!arg") {
                        //
                        // @!arg <identifier> : <type>
                        //
                        // we could validate that the identifier is not some illegal name,
                        // but this does not introduce a binding, just annotates an existing one that the user has declared in code
                        // so the checker will issue a warning if this name does not exactly match an argument for the function this binds to
                        //
                        parseTrivia();
                        const name = scanIdentifier();
                        if (!name) {
                            parseErrorAtRange(new SourceRange(pos(), pos() + 1), "Expected an identifier");
                            next();
                        }
                        else {
                            parseTrivia();
                            parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                            const type = parseType();
                            argumentAnnotations.push(NamedAnnotation(name, type));
                        }
                    }
                    else if (parseTypes && name === "!cfc-transform") {
                        eatWhitespace();
                        const namespace = parseNamespaceBody("cf_CfcTransform");
                        typedefs.push(namespace);
                    }
                    else if (parseTypes && name === "!import") {
                        eatWhitespace();
                        typedefs.push(parseTypeImport());
                    }
                    else {
                        parseErrorAtRange(start, pos(), "Invalid extended type syntax - only '@!interface', '@!typedef', '@!type' and '@!decorate' are supported.");
                        scanDocBlockAttrText(); // discard text until next attribute
                        continue;
                    }
                }
                else {
                    workingAttributeUiName = NilTerminal(start, name);
                    eatWhitespace();
                }
            }

            const text = scanDocBlockAttrText();
            const syntheticEquals = NilTerminal(workingAttributeUiName.range.toExclusive, "=");
            const textSpan = TextSpan(new SourceRange(start, scanner.getIndex()), text);
            const docBlockAttr = TagAttribute(workingAttributeUiName, workingAttributeUiName.token.text, syntheticEquals, textSpan);
            docBlockAttr.flags |= NodeFlags.docBlock;
            docBlockAttrs.push(docBlockAttr);
        }

        lastDocBlock = {
            typeAnnotation,
            typedefs,
            argumentAnnotations: argumentAnnotations.length > 0 ? argumentAnnotations : null,
            docBlockAttrs,
            treatAsConstructor
        };

        restoreScannerState(savedScannerState);
    }

    /**
     * fixme: unify this method with the docBlock parser
     * every call to this method clears the last known trivia-bound type annotation,
     * and sets it to the final type annotation of the current trivia collection, if it exists
     */
    function parseTypeAnnotationsFromPreParsedTrivia(node: Comment | CfTag.Comment) : void {
        lastDocBlock = null;

        const savedScannerState = getScannerState();
        const savedParseContext = parseContext;
        parseContext = 1 << ParseContext.typeAnnotation;

        let leadingOffset : number;
        let trailingOffset : number;
        switch (node.kind) {
            case NodeKind.comment: {
                leadingOffset = node.commentType === CommentType.scriptSingleLine
                    || node.commentType === CommentType.scriptMultiLine
                    ? 2  // script "//" or "/*"
                    : 5; // tag "<!---"
                trailingOffset = node.commentType === CommentType.scriptSingleLine
                    ? 0 // script single line no "end terminal" to skip
                    : node.commentType === CommentType.scriptMultiLine
                    ? 2 // script "*/"
                    : 4 // tag "--->"
                break;
            }
            case NodeKind.tag: {
                leadingOffset = 5; // tag "<!---"
                trailingOffset = 4; // tag "--->"
                break;
            }
            default: {
                exhaustiveCaseGuard(node);
            }
        }

        restoreScannerState({
            index: node.range.fromInclusive + leadingOffset, // jump ahead the "//" or "/*" chars
            artificialEndLimit: node.range.toExclusive - trailingOffset,
            mode: ScannerMode.tag | ScannerMode.script, // set docblock mode if we're in a docblock...
        })

        const {typedefs: defs, typeAnnotations, argumentAnnotations, treatAsConstructor} = parseTypeAnnotations();
        
        if (typeAnnotations.length > 1) {
            // we need to get the type's ranges and terminals and etc.
            // fixme: shouldn't return more than 1 annotation
            parseErrorAtRange(node.range, "Only one @type annotation is permitted per comment.");
        }

        // <whitespace><comment><whitespace><comment>
        // only the last final comment's type annotation will be considered for the next non-trivial production
        lastDocBlock = {
            typeAnnotation: typeAnnotations.length === 1 ? typeAnnotations[0] : null,
            typedefs: defs,
            argumentAnnotations,
            docBlockAttrs: [], // this is not a docblock, if it were, we'd be using "parseDocBlockFromPreParsedTrivia"
            treatAsConstructor
        };

        restoreScannerState(savedScannerState);
        parseContext = savedParseContext;
    }

    // this helps bolt on parsing type statements, which need to "emit" parse results into some collector
    function TypeStatementCollector() {
        return {
            defs: [] as (Typedef | Interfacedef | Namespace | TypeImport)[],
            typeAnnotations: [] as TypeAnnotation[], // should only be one per docblock or comment right? (T | null) not (T[])
            argumentAnnotations: [] as NamedAnnotation[],
            treatAsConstructor: false,
        }
    }
    type TypeStatementCollector = ReturnType<typeof TypeStatementCollector>;

    /**
     * fixme: this is duplicate work along w/ doing the same in a docblock
     * parse type annotations
     */
    function parseTypeAnnotations() {
        const savedContext = parseContext;
        const savedScannerMode = getScannerState().mode;
        
        // maintain "docblock" mode if we're in it? -- actually, this should not be called from docblock mode
        // it reaches out directly
        setScannerMode(ScannerMode.script | ScannerMode.tag);
        parseContext = 1 << ParseContext.typeAnnotation;
        parseTrivia();
        
        const result = TypeStatementCollector();

        while (lookahead() !== TokenType.EOF) {
            if (peek().text === "@" && peek(1).text === "!") {
                parseTypeStatement(result);
            }
            else {
                next();
                parseTrivia();
            }
        }
        
        parseContext = savedContext;
        setScannerMode(savedScannerMode);
        
        return {
            typedefs:result.defs,
            typeAnnotations: result.typeAnnotations,
            argumentAnnotations: result.argumentAnnotations.length > 0 ? result.argumentAnnotations : null,
            treatAsConstructor: result.treatAsConstructor
        }
    }

    /**
     */
    function parseTypeStatement(out: TypeStatementCollector) {
        // parse "@"
        parseExpectedTerminal(TokenType.CHAR, ParseOptions.noTrivia);
        // parse "!"
        parseExpectedTerminal(TokenType.EXCLAMATION, ParseOptions.noTrivia);
        // peek <type-kind>, we may or may not need to advance before we parse the subsequent type
        const token = peek();

        if (token.text === "interface") {
            // leave scanner alone, type parser needs "interface"
            const ifaceDef = parseType();
            if (ifaceDef.kind === TypeKind.interface) {
                out.defs.push(Interfacedef(ifaceDef));
            }
        }
        else if (token.text === "type") {
            next(), parseTrivia();
            out.typeAnnotations.push(TypeAnnotation(parseType()));
        }
        else if (token.text === "typedef") {
            next(), parseTrivia();
            const typeDef = parseTypeDef();
            out.defs.push(typeDef);
        }
        else if (token.text === "init") {
            advance(5);
            out.treatAsConstructor = true;
        }
        else if (token.text === "arg") {
            advance(3);
            parseTrivia();
            const name = scanIdentifier();
            if (!name) {
                parseErrorAtRange(new SourceRange(pos(), pos() + 1), "Expected an identifier");
                next(); // some kind of error...
            }
            else {
                parseTrivia();
                parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                const type = parseType();
                out.argumentAnnotations.push(NamedAnnotation(name, type));
            }
        }
        else if (token.text === "import") {
            advance("import".length);
            parseTrivia();
            out.defs.push(parseTypeImport());
        }
        else if (token.text === "namespace") {
            advance("namespace".length);
            parseTrivia();
            const name = parseExpectedLexemeLikeTerminal_alwaysAdvanceOnFaliure(ParseOptions.withTrivia);
            const namespace = parseNamespaceBody(name.token.text);
            out.defs.push(namespace);
        }
    }

    /**
     * should be recursive, namespaces in namespaces, support all sorts of decls in the body
     * but, we just want to support single-level namespaces that contain typedefs, at the moment
     * with the goal of support cfc-transform experiments
     */
         function parseNamespaceBody(name: string) : Namespace {
            parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
            const collector = TypeStatementCollector();
            parseList(ParseContext.typeNamespace, () => parseTypeStatement(collector));
            parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
            const namespace = Namespace(name);
            pushTypeinfoIntoTypeinfoContainer(
                collector.defs.filter(v => v.shimKind !== TypeShimKind.import), namespace.containedScope.typeinfo);
            return namespace;
        }
    
    /**
     * the syntax currently must be exactly
     * @!import "..some-path-as-a-simple-string-literal" qualified <name>
     * and we only support importing ".d.cfm" files (though this happens later in the checker)
     * 
     * later use is qualified by <name>.<something-declared-at-top-level-of-d.cfm-file>
     */
    function parseTypeImport() {
        const path = (() => {
            if (lookahead() !== TokenType.QUOTE_SINGLE && lookahead() !== TokenType.QUOTE_DOUBLE) {
                return "";
            }
            const path = parseStringLiteral();
            if (path.kind === NodeKind.interpolatedStringLiteral) {
                parseErrorAtRange(path.range, "An import path string cannot contain interpolations. This import will be ignored.");
                return ""; // pretend we got an empty string
            }
            else {
                return path.textSpan.text;
            }
        })();

        const qualified = parseExpectedLexemeLikeTerminal_alwaysAdvanceOnFaliure(ParseOptions.withTrivia);
        if (qualified.token.text !== "qualified") {
            parseErrorAtRange(qualified.range, "Expected 'qualified'.");
        }
        const name = parseExpectedLexemeLikeTerminal_alwaysAdvanceOnFaliure(ParseOptions.withTrivia);
        return TypeImport(path, name.token.text);
    }

    function parseTypeDef() : Typedef {
        const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/true, /*allowNumeric*/ false);
        if (lookahead() === TokenType.LEFT_ANGLE) {
            next();
            function parseTypeId() : cfTypeId {
                const start = pos();
                let element = parseType();
                const end = pos();
                if (element.kind !== TypeKind.typeId) {
                    parseErrorAtRange(new SourceRange(start, end), "A generic type alias parameter's names must be identifiers.");
                    return cfTypeId("<<ERROR-TYPE>>");
                }
                return element;
            }

            const params : cfTypeConstructorParam[] = [];

            while (true) {
                if (lookahead() === TokenType.RIGHT_ANGLE || lookahead() === TokenType.EOF) {
                    // next will do nothing on EOF, but will advance past RIGHT_ANGLE
                    next();
                    parseTrivia();
                    break;
                }
                // fixme: types don't have ranges
                const typeId = parseTypeId();
                if (lookahead() === TokenType.EQUAL) {
                    parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
                    params.push(TypeConstructorParam(typeId.name, parseType()));
                }
                else {
                    params.push(TypeConstructorParam(typeId.name));
                }
            }

            parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia); // discarded
            const body = parseType();
            const type = TypeConstructor(params, body);
            return Typedef(type, name.token.text);
        }
        else {
            parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia); // discarded
            const type = parseType();
            return Typedef(type, name.token.text);
        }
    }

    function parseType() : Type;
    function parseType(fromInclusive: number, toExclusive: number) : Type;
    function parseType(fromInclusive?: number, toExclusive?: number) : Type {
        // these are our annotation typenames, not cf typenames
        function tokenToType(lexeme: Token) {
            switch (lexeme.text) {
                case "numeric":
                    return BuiltinType.numeric;
                case "string":
                    return BuiltinType.string;
                case "any":
                    return BuiltinType.any;
                case "boolean":
                    return BuiltinType.boolean;
                case "true":
                    return BuiltinType.true;
                case "false":
                    return BuiltinType.false;
                case "void":
                    return BuiltinType.void;
                case "never":
                    return BuiltinType.never;
                case "null":
                    return BuiltinType.null;
                default:
                    return cfTypeId(lexeme.text);
            }
        }

        function maybeParseArrayModifier(type: Type) : Type {
            function maybeLexeme() {
                return isLexemeLikeToken(peek()) ? parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/false, /*allowNumeric*/ false) : null;
            }

            outer:
            while (true) {
                switch (lookahead()) {
                    case TokenType.LEFT_BRACKET:
                        parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                        const maybeTypeId = maybeLexeme();
                        if (maybeTypeId) {
                            let typeId : Type | null = tokenToType(maybeTypeId.token);
                            if (typeId.kind !== TypeKind.typeId) {
                                typeId = null;
                            }
                            type = cfIndexedType(type, typeId ?? cfTypeId("<<ERROR>>"));
                            parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);
                            continue outer;
                        }
                        else {
                            parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);
                            type = UninstantiatedArray(type);
                            continue outer;
                        }
                    default:
                        break;
                }
                break;
            }
            return type;
        }

        function parseTypeParamListElement() : cfTypeConstructorParam {
            const terminal = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/true, /*allowNumeric*/false);
            const defaultType = lookahead() === TokenType.EQUAL
                ? parseType()
                : undefined;

            if (lookahead() !== TokenType.RIGHT_ANGLE) {
                parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.withTrivia);
            }

            return TypeConstructorParam(terminal.token.text, defaultType);
        }

        const enum StructMemberParseResult_t { keyTypePair, spread, indexSignature };
        type StructMemberParseResult =
            | {resultKind: StructMemberParseResult_t.keyTypePair, name: Terminal, type: Type, optional: boolean}
            | {resultKind: StructMemberParseResult_t.indexSignature, indexSignature: IndexSignature}
            | {resultKind: StructMemberParseResult_t.spread, type: Type };

        function parseTypeStructMemberElement() : StructMemberParseResult {
            let name : Terminal | null;
            let type : Type;

            if (lookahead() === TokenType.DOT_DOT_DOT) {
                parseExpectedTerminal(TokenType.DOT_DOT_DOT, ParseOptions.withTrivia);
                name = null;
                type = parseType();
                parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
                return {
                    resultKind: StructMemberParseResult_t.spread,
                    type
                }
            }
            else if (lookahead() === TokenType.LEFT_BRACKET) {
                parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                const name = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia).token.text;
                parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);

                const start = pos();
                let indexType = parseType();
                const end = pos();

                if (indexType !== BuiltinType.string && indexType !== BuiltinType.numeric) {
                    parseErrorAtRange(new SourceRange(start, end), "An index signature parameter type must be 'string' or 'numeric'");
                    type = BuiltinType.numeric;
                }

                parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);
                parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                type = parseType();
                parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

                return {
                    resultKind: StructMemberParseResult_t.indexSignature,
                    indexSignature: {
                        name,
                        indexType,
                        type
                    }
                }
            }
            else {
                name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ false, /*allowNumeric*/ true);
                // `name` is always truthy, though
                // if (!name) {
                //     parseErrorAtCurrentToken("Expected a struct key.");
                // }

                const optional = !!parseOptionalTerminal(TokenType.QUESTION_MARK, ParseOptions.withTrivia);
                parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                type = parseType();

                parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);

                return {
                    resultKind: StructMemberParseResult_t.keyTypePair,
                    name,
                    type,
                    optional,
                }
            }
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
        
        parseTrivia(); // only necessary if we just updated scanner state? caller can maybe guarantee that the target scanner state begins on non-trivial input?

        if (!isStartOfType()) {
            parseErrorAtPos(pos(), "Expected a type expression.");
            next(), parseTrivia();
            return BuiltinType.any;
        }

        function localParseType() : Type {
            let result : Type = BuiltinType.any;

            switch (lookahead()) {
                case TokenType.LEXEME: {
                    const terminal = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);

                    if (!isInSomeContext(ParseContext.interface) && terminal.token.text === "interface") {
                        const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ false);
                        let typeParams : cfTypeConstructorParam[] | undefined;
                        if (lookahead() === TokenType.LEFT_ANGLE) {
                            parseExpectedTerminal(TokenType.LEFT_ANGLE, ParseOptions.withTrivia);
                            typeParams = parseList(ParseContext.typeParamList, parseTypeParamListElement);
                            parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.withTrivia);
                        }

                        const start = pos();
                        const def = doInExtendedContext(ParseContext.interface, parseType);
                        const end = pos();

                        if (!isStructLike(def) || def.kind !== TypeKind.struct) {
                            parseErrorAtRange(new SourceRange(start, end), "Expected an interface definition");
                        }

                        let members : ReadonlyMap<string, Readonly<SymTabEntry>> | undefined;
                        let indexSignature : IndexSignature | undefined;
                        let spreads : readonly Type[] | undefined;

                        if (def.kind === TypeKind.struct) {
                            members = def.members;
                            indexSignature = def.indexSignature;
                            // @instantiableSpreads
                            // spreads = def.instantiableSpreads;
                        }

                        return Interface(
                            name.token.text,
                            members ?? new Map(),
                            typeParams,
                            indexSignature,
                            spreads);
                    }
                    else {
                        result = tokenToType(terminal.token);
                        if (terminal.token.text.toLowerCase() === "cfc") {
                            parseExpectedTerminal(TokenType.LEFT_ANGLE, ParseOptions.withTrivia);
                            const dottedPath = parseDottedPath();
                            parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.withTrivia);
                            const cfcPathNameAsLiteralType = createLiteralType(stringifyDottedPath(dottedPath).ui);
                            result = CfcLookup(cfcPathNameAsLiteralType);
                        }
                        else if (result.kind === TypeKind.typeId) {
                            result = (() => {
                                const rest = parseTypeIdRest();
                                return rest.length === 0 ? result : cfTypeId(result.name, rest);
                            })();

                            if (peek().type === TokenType.LEXEME && peek().text === "extends") {
                                next();
                                parseTrivia();

                                const extends_ = doInExtendedContext(ParseContext.typeExtends, parseType);

                                parseExpectedTerminal(TokenType.QUESTION_MARK, ParseOptions.withTrivia);
                                const consequent = parseType();
                                parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                                const alternative = parseType();
                                result = cfConditionalType(result, extends_, consequent, alternative);
                                break;
                            }
                            else if (lookahead() === TokenType.LEFT_ANGLE) {
                                parseExpectedTerminal(TokenType.LEFT_ANGLE, ParseOptions.withTrivia);
                                const args : Type[] = [];
                                while (lookahead() !== TokenType.RIGHT_ANGLE && lookahead() !== TokenType.EOF) {
                                    args.push(parseType());
                                    if (lookahead() !== TokenType.RIGHT_ANGLE) {
                                        parseExpectedTerminal(TokenType.COMMA, ParseOptions.withTrivia);
                                    }
                                }
                                parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.withTrivia);
                                result = TypeConstructorInvocation(result, args);
                            }
                        }

                        result = maybeParseArrayModifier(result);
                    }
                    break;
                }
                case TokenType.LEFT_PAREN: {
                    parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                    
                    // this might be a parameter list to a function type, or just a parenthesized type
                    // we need some lookahead to know which
                    const enum TypeOrParam { type, param };
                    const typeOrParam = SpeculationHelper.lookahead(() => {
                        const nextToken = next();
                        parseTrivia();
                        
                        if (nextToken.type === TokenType.DOT_DOT_DOT
                            || (nextToken.type === TokenType.RIGHT_PAREN && peek().type === TokenType.EQUAL_RIGHT_ANGLE)
                            || (isLexemeLikeToken(nextToken)
                            && (lookahead() === TokenType.COLON || (lookahead() === TokenType.QUESTION_MARK && peek(1).type === TokenType.COLON))))
                        {
                            // this is a non-type name, used to give a name to an inline function type signature parameter,
                            // i.e, (foo : any) => any
                            //       ^^^^^
                            // (...foo: bar) => any
                            //  ^^^^^^
                            // () => any
                            // ^^^^^
                            return TypeOrParam.param;
                        }

                        return TypeOrParam.type;
                    })

                    if (typeOrParam === TypeOrParam.type) {
                        result = parseType();
                        parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                        result = maybeParseArrayModifier(result);
                        break;
                    }
                    else {
                        result = fixme__parseArrowFunctionTypeAssumingFirstParenIsAlreadyParsed(/*asGenericFunction*/ false);
                        break;
                    }
                }
                case TokenType.LEFT_BRACE: {
                    // function parseMappedTypeKeyExpression() : {keyBinding: cfTypeId, inKeyOf: cfTypeId} {
                    //     parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                    //     const name = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ false);
                    //     let keyBinding : _Type | null = textToType(name.token);
                    //     if (!isTypeId(keyBinding)) { // got number or string or etc.
                    //         parseErrorAtRange(name.range, "Mapped type key binding must be a valid typename.");
                    //         keyBinding = null;
                    //     }
                    //     const contextualKwIn = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ false);
                    //     const contextualKwKeyof = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ false);
                    //     if (contextualKwIn.token.text !== "in" || contextualKwKeyof.token.text !== "keyof") {
                    //         parseErrorAtRange(mergeRanges(contextualKwIn, contextualKwKeyof), "Expected 'in keyof'.");
                    //     }
                    //     const inKeyOfName = parseExpectedLexemeLikeTerminal(/*consumeOnFailure*/ true, /*allowNumeric*/ false);
                    //     let inKeyOfBinding : _Type | null = textToType(inKeyOfName.token);
                    //     if (!isTypeId(inKeyOfBinding)) { // got number or string or etc.
                    //         parseErrorAtRange(name.range, "Mapped type keyof source must be a valid typename.");
                    //         inKeyOfBinding = null;
                    //     }
                    //     parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);
                        
                    //     return {
                    //         keyBinding: keyBinding ?? cfTypeId("<<ERROR>>"),
                    //         inKeyOf: inKeyOfBinding ?? cfTypeId("<<ERROR>>")
                    //     }
                    // }

                    parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);

                    // if (lookahead() === TokenType.LEFT_BRACKET) {
                    //     const {keyBinding, inKeyOf} = parseMappedTypeKeyExpression();
                    //     parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                    //     const type = parseType();
                    //     result = cfMappedType(keyBinding, inKeyOf, type);
                    // }
                    // else {
                        const kvPairs = parseList(ParseContext.typeStruct, parseTypeStructMemberElement);
                        parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);

                        const members = new Map<string, SymTabEntry>();
                        const instantiableSpreads : Type[] = [];
                        let indexSignature : IndexSignature | undefined = undefined;
                        for (const member of kvPairs) {
                            if (member.resultKind === StructMemberParseResult_t.spread) {
                                instantiableSpreads.push(member.type);
                                continue;
                            }
                            else if (member.resultKind === StructMemberParseResult_t.indexSignature) {
                                indexSignature = member.indexSignature;
                                continue;
                            }
                            else {
                                members.set(member.name.token.text.toLowerCase(), {
                                    uiName: member.name.token.text,
                                    flags: 0,
                                    canonicalName: member.name.token.text.toLowerCase(),
                                    declarations: null,
                                    lexicalType: member.type,
                                    effectivelyDeclaredType: undefined,
                                    links: {
                                        optional: member.optional,
                                    },
                                    symbolId: -1,
                                })
                            }
                        }

                        result = Struct(members, indexSignature, instantiableSpreads);
                    // }

                    // fixme: needs to happen at a higher level; we don't want to support `interface {}[]`
                    result = maybeParseArrayModifier(result);

                    break;
                }
                case TokenType.LEFT_ANGLE: {
                    parseExpectedTerminal(TokenType.LEFT_ANGLE, ParseOptions.withTrivia);
                    const typeParams = parseList(ParseContext.typeParamList, parseTypeParamListElement);
                    parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.withTrivia);
                    parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                    return fixme__parseArrowFunctionTypeAssumingFirstParenIsAlreadyParsed(/*asGenericFunction*/true, typeParams);
                }
                case TokenType.LEFT_BRACKET: {
                    parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                    const elements : SymTabEntry[] = [];
                    let first = true;
                    while (true) {
                        if (!first) {
                            parseExpectedTerminal(TokenType.COMMA, ParseOptions.withTrivia);
                        }
                        const nameOrElement = parseExpectedLexemeLikeTerminal_alwaysAdvanceOnFaliure(ParseOptions.withTrivia);
                        if (lookahead() === TokenType.COLON) {
                            parseExpectedTerminal(TokenType.COLON, ParseOptions.withTrivia);
                            const type = parseType();
                            elements.push({
                                canonicalName: nameOrElement.token.text.toLowerCase(),
                                uiName: nameOrElement.token.text,
                                flags: 0,
                                declarations: null,
                                lexicalType: type,
                                effectivelyDeclaredType: undefined,
                                symbolId: -1
                            })
                        }
                        else {
                            const type = tokenToType(nameOrElement.token);
                            elements.push({
                                canonicalName: "",
                                uiName: "",
                                flags: 0,
                                declarations: null,
                                lexicalType: type,
                                effectivelyDeclaredType: undefined,
                                symbolId: -1
                            })
                        }

                        if (lookahead() === TokenType.RIGHT_BRACKET || lookahead() === TokenType.EOF) {
                            next();
                            parseTrivia();
                            break;
                        }

                        first = false;
                    }
                    return cfTuple(elements);
                }
                case TokenType.QUOTE_SINGLE:
                case TokenType.QUOTE_DOUBLE: {
                    const s = doInExtendedContext(ParseContext.typeString, () => parseStringLiteral(StringUniverse.type));
                    if (s.kind === NodeKind.simpleStringLiteral) {
                        result = createLiteralType(s.textSpan.text);
                    }
                    else {
                        result = cfInterpolatedString(s);
                    }
                    break;
                }
                case TokenType.NUMBER: {
                    const n = parseNumericLiteral();
                    const value = parseFloat(n.literal.token.text);
                    result = createLiteralType(value);
                }
                default: {
                    if (isValidTypeIdOrBuiltinTypename(lookahead())) {
                        const token = next();
                        parseTrivia();
                        result = tokenToType(token);
                    }
                }
            }

            return result;
        }

        function fixme__parseArrowFunctionTypeAssumingFirstParenIsAlreadyParsed(asGenericFunction: true, typeParams: cfTypeConstructorParam[]) : cfGenericFunctionSignature;
        function fixme__parseArrowFunctionTypeAssumingFirstParenIsAlreadyParsed(asGenericFunction: false) : cfFunctionSignature;
        function fixme__parseArrowFunctionTypeAssumingFirstParenIsAlreadyParsed(asGenericFunction: boolean, typeParams?: cfTypeConstructorParam[]) {
            const params = parseFunctionTypeParametersArrowLike();
            parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
            parseExpectedTerminal(TokenType.EQUAL_RIGHT_ANGLE, ParseOptions.withTrivia);
            const returnType = parseType();
            if (asGenericFunction) {
                return cfGenericFunctionSignature("", typeParams!, params, returnType, []);
            }
            else {
                return cfFunctionSignature("", params, returnType, []);
            }
        }

        // in `type T = | A` --- we have a union of one element, which gets collapsed to A anyway (and we'll actually just parse exactly A), so this seems legit
        // makes multiline unions nicer
        parseOptionalTerminal(TokenType.PIPE, ParseOptions.withTrivia); // discarded, not correct if we just parsed an interface or namespace or other non-unionable thing
        let result = localParseType();

        if (isStructLike(result) && result.kind === TypeKind.interface) {
            return result;
        }

        intersectionsAndUnions:
        while (true) {
            switch (lookahead()) {
                case TokenType.AMPERSAND: {
                    next(), parseTrivia();
                    if (result.kind === TypeKind.intersection) {
                        // we'll parse it but we don't do anything with it
                        //result.types.push(localParseType());
                        localParseType();
                    }
                    else {
                        // not supporting intersections just yet
                        // result = cfIntersection(result, localParseType());
                        localParseType();
                    }
                    break;
                }
                case TokenType.PIPE: {
                    next(), parseTrivia();
                    if (result.kind === TypeKind.union) {
                        (result as Mutable<cfUnion>).types = [...result.types, localParseType()];
                    }
                    else {
                        result = cfUnion(new Set([result, localParseType()]));
                    }
                    break;
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

    function canParseExplicitInfer() {
        const inStringInExtends = (parseContext & 1 << ParseContext.typeExtends) && (parseContext & 1 << ParseContext.typeString);
        return inStringInExtends;
    }

    /**
     * explicit infer is like "abc#infer rest#", for now only useful in the extends position of a conditional type
     */
    function parseTypeIdWithMaybeExplicitInfer(allowExplicitInfer = true) : cfTypeId | cfTypeConstructorInvocation {
        const terminal = parseExpectedLexemeLikeTerminal_alwaysAdvanceOnFaliure(ParseOptions.withTrivia, /*allowNumeric*/ false, "Expected a typename.");
        if (allowExplicitInfer && canParseExplicitInfer() && terminal.token.text === "infer") {
            const name = parseExpectedLexemeLikeTerminal_alwaysAdvanceOnFaliure(ParseOptions.withTrivia, /*allowNumeric*/ false, "Expected a typename.");
            return cfInferenceTarget(name.token.text);
        }
        else {
            const typeId = cfTypeId(terminal.token.text, parseTypeIdRest());
            if (lookahead() === TokenType.LEFT_ANGLE) {
                parseExpectedTerminal(TokenType.LEFT_ANGLE, ParseOptions.withTrivia);

                const args : Type[] = [];
                while (lookahead() !== TokenType.RIGHT_ANGLE && lookahead() !== TokenType.EOF) {
                    args.push(parseTypeIdWithMaybeExplicitInfer(/*allowExplicitInfer*/ false));
                    if (lookahead() !== TokenType.RIGHT_ANGLE) {
                        parseExpectedTerminal(TokenType.COMMA, ParseOptions.withTrivia);
                    }
                }
                parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.withTrivia);
                return TypeConstructorInvocation(typeId, args);
            }
            else {
                return typeId;
            }
        }
    }

    function parseTypeIdRest() {
        const result : cfTypeId[] = [];
        while (lookahead() !== TokenType.EOF) {
            parseTrivia();
            if (lookahead() === TokenType.DOT) {
                parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
                const propertyName = parseExpectedLexemeLikeTerminal_alwaysAdvanceOnFaliure(ParseOptions.withTrivia, /*allowNumeric*/false, "Expected a typename.");
                result.push(cfTypeId(propertyName.token.text));
            }
            else {
                break;
            }
        }
        return result;
    }
}

export type Parser = ReturnType<typeof Parser>;