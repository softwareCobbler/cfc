import { CfTag, Node, NodeType, TagAttribute, NodeFlags, Terminal, Comment, TextSpan, NilTerminal,
    Conditional, ConditionalSubtype, FunctionParameter, FromTag, FunctionDefinition, CommentType,
    HashWrappedExpr, Assignee, Assignment, BinaryOperator, Parenthetical, UnaryOperator, BooleanLiteral,
    CallExpression, IndexedAccess, pushAccessElement, CallArgument, Identifier, SimpleStringLiteral, InterpolatedStringLiteral,
    NumericLiteral, DottedPath, ArrowFunctionDefinition, Statement, Block } from "./node";
import { SourceRange } from "./scanner";
import { Token, TokenType, TokenizerMode, Tokenizer, TokenTypeUiString } from "./tokenizer";
import { allowTagBody, isLexemeLikeToken, requiresEndTag, getAttributeValue, getTriviallyComputableBoolean, getTriviallyComputableString } from "./utils";

const enum ParseOptions {
    none     = 0,
    noTrivia = 0,
    withTrivia        = 0x00000001,
    allowHashWrapped  = 0x00000002,
};

const enum ParseFlags {
    none = 0,
    inHashWrappedExpr = 0x00000001
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
export function Parser(tokenizer_: Tokenizer, mode_: TokenizerMode = TokenizerMode.tag) {
    let tokenizer : Tokenizer = tokenizer_;
    let mode: TokenizerMode = mode_;
    let parseFlags : ParseFlags = ParseFlags.none;
    let lookahead_ : TokenType = peek().type;
    let globalDiagnosticEmitter : (() => void) | null = null;

    const diagnostics : Diagnostic[] = [];
    let parseErrorBeforeNextFinishedNode = false;
    parseErrorBeforeNextFinishedNode;

    function peek(jump: number = 0) {
        return tokenizer.peek(jump, mode);
    }

    function lookahead() {
        return lookahead_;
    }

    function next() {
        const result = tokenizer.next(mode);
        lookahead_ = peek().type;
        return result;
    }

    function getTokenizerState() : TokenizerState {
        return {
            index: tokenizer.getIndex(),
            mode: mode,
            artificialEndLimit: tokenizer.getArtificalEndLimit()
        }
    }

    function restoreTokenizerState(state: TokenizerState) {
        tokenizer.restoreIndex(state.index);
        mode = state.mode;
        if (state.artificialEndLimit) {
            tokenizer.setArtificialEndLimit(state.artificialEndLimit);
        }
        else {
            tokenizer.clearArtificalEndLimit();
        }
        lookahead_ = peek().type;
    }

    function tagMode() : boolean {
        return mode === TokenizerMode.tag;
    }
    function scriptMode() : boolean {
        return mode === TokenizerMode.script;
    }
    function inHashWrappedExpr() : boolean {
        return !!(parseFlags & ParseFlags.inHashWrappedExpr);
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
        const sourceRange = peek().range;
        parseErrorAtRange(sourceRange.fromInclusive, sourceRange.toExclusive, msg);
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
        throw "nyi";
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
            return CfTag.Common(CfTag.Which.start, tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, tagAttrs);
        }
    }

    function parseCfEndTag() {
        const tagStart = parseExpectedTerminal(TokenType.CF_END_TAG_START, ParseOptions.noTrivia);
        const tagName = parseExpectedTagName();
        const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);

        let canonicalName = "";
        if (!(tagName.flags & NodeFlags.missing)) {
            canonicalName = tokenizer.getTokenText(tagName.token).toLowerCase();
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
                result.push(TagAttribute(attrName, tokenizer_.getTokenText(attrName.token).toLowerCase(), equal, value));
            }
            else {
                result.push(TagAttribute(attrName, tokenizer_.getTokenText(attrName.token).toLowerCase()));
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
                let root = Conditional(ConditionalSubtype.if, ifTag, rootConsequent);

                let working = root;


                while (true) {
                    const elseIfTag = parseOptionalTag(CfTag.Which.start, "elseif");
                    if (elseIfTag) {
                        const consequent = treeifyTags(reductionInstructions.cfelseif);
                        working.alternative = Conditional(ConditionalSubtype.elseif, elseIfTag, consequent);
                        working = root.alternative!;
                        continue;
                    }
                    const elseTag = parseOptionalTag(CfTag.Which.start, "else");
                    if (elseTag) {
                        const consequent = treeifyTags(reductionInstructions.cfelse);
                        working.alternative = Conditional(ConditionalSubtype.else, elseTag, consequent);
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
                return FunctionDefinition(startTag, params, body, endTag, functionName);
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
                                // same as handling set, except there is an end tag to consume
                                const stmtList = (<CfTag.Script>tag).stmtList;
                                nextTag();
                                const endTag = parseExpectedTag(CfTag.Which.end, "script");
                                result.push(FromTag.Block(tag, stmtList, endTag));
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
                const index = tokenizer.getIndex();
                tagTextRange = new SourceRange(index, index+1);
            }
        }
        function finishTagTextRange() {
            if (tagTextRange.isNil()) {
                return;
            }
            tagTextRange.toExclusive = tokenizer.getIndex(); // does not include current; so, no "+1"
            result.push(CfTag.Text(tagTextRange))
            tagTextRange = SourceRange.Nil();
        }

        while (lookahead_ != TokenType.EOF) {
            switch (lookahead_) {
                case TokenType.CF_START_TAG_START: {
                    finishTagTextRange();
                    const tag = parseCfStartTag();
                    if (tag.canonicalName === "script") {
                        // parse cfscript ...
                    }
                    else {
                        result.push(tag);
                    }
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
            return TextSpan(range, tokenizer_.getTextSlice(range));
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
        const result : (TextSpan | HashWrappedExpr)[] = [];
        let textSourceRange = SourceRange.Nil();

        function startOrContinueTextRange() {
            if (textSourceRange.isNil()) {
                const index = tokenizer.getIndex();
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
            textSourceRange.toExclusive = tokenizer.getIndex(); // current index is NOT included, so, no '+1'
            result.push(TextSpan(textSourceRange, tokenizer_.getTextSlice(textSourceRange)));
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
                parseErrorAtRange(root.range, "final/var declaration modifier on non-declaring expression");
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
            throw "nyi";
            //return parseAnonymousFunctionDefinition();
        }

        const maybeArrowFunction = SpeculationHelper.speculate(tryParseArrowFunctionDefinition);
        if (maybeArrowFunction) {
            return maybeArrowFunction;
        }

        return parseExpression();
    }

    function parseExpression() : Node {
        const saveDiagnosticEmitter = globalDiagnosticEmitter;
        const currentPos = tokenizer_.getIndex();
        globalDiagnosticEmitter = () => parseErrorAtRange(currentPos, tokenizer_.getIndex(), "Expected an expression");

        let root = parseBooleanExpression();

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
                    const right = parseExpression();
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

    function parseBooleanExpression(descendIntoOr = true) : Node { // i think this binds the &&'s correctly
        let root = parseAddition();

        outer:
        while (true) {
            switch (lookahead()) {
                case TokenType.DBL_PIPE:
                case TokenType.LIT_OR:
                case TokenType.LIT_XOR: {
                    if (!descendIntoOr) break outer;
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseAddition();
                    root = BinaryOperator(root, op, expr);
                    continue;
                }
                case TokenType.DBL_AMPERSAND:
                case TokenType.LIT_AND: {
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseBooleanExpression(/*descendIntoOr*/ false);
                    root = BinaryOperator(root, op, expr);
                    continue;
                }
            }
            // if we didn't match any of the above operators, we're done
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

    function parseParentheticalOrUnaryPrefix() : Node {
        switch (lookahead()) {
            case TokenType.LEFT_PAREN: {
                const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                const expr = parseExpression();
                const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                return Parenthetical(leftParen, expr, rightParen);
            }
            case TokenType.DBL_MINUS: // [[fallthrough]];
            case TokenType.DBL_PLUS: {
                const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                const expr = parseCallExpressionOrLower();
                return UnaryOperator(op, expr);
            }
            default: return parseCallExpressionOrLower();
        }
    }

    function parseHashWrappedExpression() {
        if (inHashWrappedExpr()) throw "parseHashWrappedExpr cannot be nested";

        parseFlags |= ParseFlags.inHashWrappedExpr;

        const leftHash = parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);
        const expr = parseExpression();
        const rightHash = parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia, "Unterminated hash-wrapped expression");

        parseFlags &= ~ParseFlags.inHashWrappedExpr;

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
            case TokenType.HASH:
                if (!inHashWrappedExpr()) {
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
                case TokenType.LEFT_BRACE:
                case TokenType.DOT:
                    root = parseIndexedAccess(root);
                    continue;
                case TokenType.LEFT_PAREN:
                    root = parseCallExpression(root);
                    continue;
            }
            break;
        }

        if (root instanceof CallExpression) {
            switch (lookahead()) {
                case TokenType.DBL_PLUS:
                case TokenType.DBL_MINUS:
                    const unaryOp = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    root = UnaryOperator(root, unaryOp);
            }
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

    function parseIdentifier(parseOptions : ParseOptions.none) : Identifier;
    function parseIdentifier(parseOptions : ParseOptions) : Identifier | HashWrappedExpr;
    function parseIdentifier(parseOptions : ParseOptions) : Identifier | HashWrappedExpr {
        let leftHash : Terminal | null = null;
        if (parseOptions & ParseOptions.allowHashWrapped && !inHashWrappedExpr()) {
            leftHash = parseOptionalTerminal(TokenType.HASH, ParseOptions.withTrivia);
        }

        const identifier = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
        const name = tokenizer.getTokenText(identifier.token);

        if (parseOptions & ParseOptions.allowHashWrapped && leftHash) {
            const rightHash = parseExpectedTerminal(
                TokenType.HASH,
                ParseOptions.withTrivia,
                () => parseErrorAtRange(
                    leftHash!.range.fromInclusive,
                    tokenizer_.getIndex(),
                    "Unterminated hash wrapped expression"));

            return HashWrappedExpr(
                leftHash,
                Identifier(identifier, name),
                rightHash);
        }
        else {
            return Identifier(identifier, name);
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
                return SimpleStringLiteral(quoteType, leftQuote, onlyElement, rightQuote);
            }
        }

        return InterpolatedStringLiteral(quoteType, leftQuote, stringElements, rightQuote);
    }

    function parseNumericLiteral() {
        return NumericLiteral(parseExpectedTerminal(TokenType.NUMBER, ParseOptions.withTrivia));
    }

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
        function speculate<T>(speculationWorker: () => T) : T | null {
            const saveTokenizerState = getTokenizerState();
            const saveParseErrorBeforeNextFinishedNode = parseErrorBeforeNextFinishedNode;

            const result = speculationWorker();

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

    function isJavaLikeTypenameThenName() : boolean {
        if (lookahead() !== TokenType.LEXEME) {
            return false;
        }

        next();

        while (lookahead() === TokenType.DOT) {
            next();
            if (lookahead() === TokenType.LEXEME) next();
            else return false;    
        }

        parseTrivia();
        return lookahead() === TokenType.LEXEME;
    }

    function isIdentifierThenFatArrow() : boolean {
        if (lookahead() !== TokenType.LEXEME) return false;
        next();
        parseTrivia();
        if (lookahead() !== TokenType.EQUAL_RIGHT_ANGLE) return false;
        return true;
    }

    function parseDottedPathTypename() : DottedPath<Terminal> {
        const result = DottedPath<Terminal>(parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia));
        while (lookahead() === TokenType.DOT) {
            result.rest.push({
                dot: parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia),
                name: parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia)
            })
        }
        return result;
    }

    function tryParseFunctionDefinitionParameters() : FunctionParameter[] | null {
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
                name = parseIdentifier(ParseOptions.none);
            }
            // function foo((required)? name (= defaultExpr)?) { ... }
            //                          ^^^^
            else if (lookahead() === TokenType.LEXEME) {
                name = parseIdentifier(ParseOptions.none);
            }
            // didn't match anything
            else {
                return null;
            }
            
            if (lookahead() === TokenType.LEXEME) {
                const discardedLexemesStartPos = tokenizer_.getIndex();
                do {
                    next();
                } while (lookahead() === TokenType.LEXEME);
                
                // @fixme: should be a warning
                parseErrorAtRange(discardedLexemesStartPos, tokenizer_.getIndex(), "Names in this position will be discarded at runtime; are you missing a comma?")
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
        let fatArrow : Terminal;

        if (SpeculationHelper.lookahead(isIdentifierThenFatArrow)) {
            params = [
                FunctionParameter(
                    null,
                    null,
                    parseIdentifier(ParseOptions.none),
                    null,
                    null,
                    null)
            ];
        }
        else if (lookahead() === TokenType.LEFT_PAREN) {
            leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
            const maybeParams = SpeculationHelper.speculate(tryParseFunctionDefinitionParameters);
            if (!maybeParams) {
                return null;
            }
            params = maybeParams;
            rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        }
        else {
            return null;
        }

        fatArrow = parseExpectedTerminal(TokenType.EQUAL_RIGHT_ANGLE, ParseOptions.withTrivia);

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

    function parseStatementList(stopToken: TokenType = TokenType.RIGHT_BRACE) : Statement[] {
        const result : Statement[] = [];

        do {
            result.push(parseStatement());
        } while (lookahead() !== stopToken && lookahead() !== TokenType.EOF);

        return result;
    }

    function parseStatement() : Statement {
        while (lookahead() !== TokenType.EOF) {
            switch (lookahead()) {
                case TokenType.LEFT_BRACE: {
                    const leftBrace = parseExpectedTerminal(TokenType.LEFT_BRACE, ParseOptions.withTrivia);
                    const body = parseStatementList();
                    const rightBrace = parseExpectedTerminal(TokenType.RIGHT_BRACE, ParseOptions.withTrivia);
                    const semicolon = null;
                    return Statement(
                        Block(leftBrace, body, rightBrace),
                        semicolon);
                }
                case TokenType.NUMBER: {
                    const expr = parseExpression();
                    const semicolon = scriptMode() ? parseOptionalTerminal(TokenType.SEMICOLON, ParseOptions.withTrivia) : null;
                    return Statement(expr, semicolon);
                }
                default: throw "unsupported statement / nyi";
            }
        }

        throw "should never get here -- hit EOF while parsing a statement";
    }

    function getDiagnostics() : readonly Diagnostic[] {
        return diagnostics;
    }

    return {
        parseTags,
        getDiagnostics
    }
}
