/******/ (() => { // webpackBootstrap
/******/ 	var __webpack_modules__ = ({

/***/ "./out/compiler/index.js":
/*!*******************************!*\
  !*** ./out/compiler/index.js ***!
  \*******************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Parser = exports.Tokenizer = exports.Scanner = void 0;
var scanner_1 = __webpack_require__(/*! ./scanner */ "./out/compiler/scanner.js");
Object.defineProperty(exports, "Scanner", ({ enumerable: true, get: function () { return scanner_1.Scanner; } }));
var tokenizer_1 = __webpack_require__(/*! ./tokenizer */ "./out/compiler/tokenizer.js");
Object.defineProperty(exports, "Tokenizer", ({ enumerable: true, get: function () { return tokenizer_1.Tokenizer; } }));
var parser_1 = __webpack_require__(/*! ./parser */ "./out/compiler/parser.js");
Object.defineProperty(exports, "Parser", ({ enumerable: true, get: function () { return parser_1.Parser; } }));
//# sourceMappingURL=index.js.map

/***/ }),

/***/ "./out/compiler/parser.js":
/*!********************************!*\
  !*** ./out/compiler/parser.js ***!
  \********************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Parser = exports.CfTag = void 0;
const scanner_1 = __webpack_require__(/*! ./scanner */ "./out/compiler/scanner.js");
const tokenizer_1 = __webpack_require__(/*! ./tokenizer */ "./out/compiler/tokenizer.js");
const tagFacts = {
    // the "cf" prefix is implied
    abort: 2 /* DISALLOW_BODY */,
    application: 2 /* DISALLOW_BODY */,
    argument: 2 /* DISALLOW_BODY */,
    break: 2 /* DISALLOW_BODY */,
    catch: 2 /* DISALLOW_BODY */,
    content: 2 /* DISALLOW_BODY */,
    continue: 2 /* DISALLOW_BODY */,
    cookie: 2 /* DISALLOW_BODY */,
    directory: 2 /* DISALLOW_BODY */,
    documentitem: 2 /* DISALLOW_BODY */,
    dump: 2 /* DISALLOW_BODY */,
    error: 2 /* DISALLOW_BODY */,
    exit: 2 /* DISALLOW_BODY */,
    file: 1 /* ALLOW_BODY */,
    function: 4 /* REQUIRE_BODY */,
    header: 2 /* DISALLOW_BODY */,
    http: 1 /* ALLOW_BODY */,
    httpparam: 2 /* DISALLOW_BODY */,
    include: 2 /* DISALLOW_BODY */,
    input: 2 /* DISALLOW_BODY */,
    invoke: 1 /* ALLOW_BODY */,
    invokeargument: 2 /* DISALLOW_BODY */,
    location: 2 /* DISALLOW_BODY */,
    log: 2 /* DISALLOW_BODY */,
    mail: 4 /* REQUIRE_BODY */,
    mailparam: 2 /* DISALLOW_BODY */,
    output: 4 /* REQUIRE_BODY */,
    param: 2 /* DISALLOW_BODY */,
    pdf: 1 /* ALLOW_BODY */,
    pdfform: 2 /* DISALLOW_BODY */,
    pdfformparam: 2 /* DISALLOW_BODY */,
    pdfparam: 2 /* DISALLOW_BODY */,
    procparam: 2 /* DISALLOW_BODY */,
    procresult: 2 /* DISALLOW_BODY */,
    property: 2 /* DISALLOW_BODY */,
    queryparam: 2 /* DISALLOW_BODY */,
    query: 4 /* REQUIRE_BODY */,
    reportparam: 2 /* DISALLOW_BODY */,
    rethrow: 2 /* DISALLOW_BODY */,
    return: 2 /* DISALLOW_BODY */,
    set: 2 /* DISALLOW_BODY */,
    setting: 2 /* DISALLOW_BODY */,
    spreadsheet: 2 /* DISALLOW_BODY */,
    storedproc: 1 /* ALLOW_BODY */,
    throw: 2 /* DISALLOW_BODY */,
    trace: 2 /* DISALLOW_BODY */,
    transaction: 1 /* ALLOW_BODY */,
    wddx: 1 /* ALLOW_BODY */,
    zip: 1 /* ALLOW_BODY */,
    zipparam: 2 /* DISALLOW_BODY */,
};
function getTagFacts(tag) {
    if (tagFacts.hasOwnProperty(tag.canonicalName)) {
        return tagFacts[tag.canonicalName];
    }
    return null;
}
function requiresEndTag(tag) {
    const facts = getTagFacts(tag);
    return !!facts && !!(facts & 4 /* REQUIRE_BODY */);
}
function allowTagBody(tag) {
    const facts = getTagFacts(tag);
    return !!facts && ((facts & 1 /* ALLOW_BODY */) || !!(facts & 4 /* REQUIRE_BODY */));
}
class NodeBase {
    constructor(sourceRange) {
        this.flags = 0 /* none */;
        this.parent = null;
        this.range = sourceRange,
            this.tagOrigin = { startTag: null, endTag: null };
    }
}
function mergeRanges(...nodes) {
    const result = scanner_1.SourceRange.Nil();
    if (nodes.length === 0) {
        return result;
    }
    let gotStart = false;
    for (const node of nodes) {
        if (!node)
            continue;
        let thisRange;
        if (node instanceof NodeList) {
            thisRange = mergeRanges(...node.list);
        }
        else {
            thisRange = node.range;
        }
        if (!gotStart) {
            result.fromInclusive = thisRange.fromInclusive;
            gotStart = true;
        }
        else {
            result.toExclusive = thisRange.toExclusive;
        }
    }
    return result;
}
class NodeList extends NodeBase {
    constructor(list = []) {
        super(scanner_1.SourceRange.Nil());
        this.list = list;
    }
    Nil() {
        return new NodeList();
    }
}
class Terminal extends NodeBase {
    constructor(token, trivia) {
        super(token.range);
        this.token = token;
        this.trivia = trivia !== null && trivia !== void 0 ? trivia : new NodeList();
    }
    static Nil() {
        return new Terminal(new tokenizer_1.Token(0 /* NIL */, scanner_1.SourceRange.Nil()));
    }
}
class TextSpan extends NodeBase {
    // all we're interested in is range
    constructor(sourceRange) {
        super(sourceRange);
    }
}
class HashWrappedExpr extends NodeBase {
    constructor(leftHash, expr, rightHash) {
        super(mergeRanges(leftHash, expr, rightHash));
        this.leftHash = leftHash;
        this.expr = expr;
        this.rightHash = rightHash;
    }
}
class Parenthetical extends NodeBase {
    constructor(leftParen, expr, rightParen) {
        super(mergeRanges(leftParen, expr, rightParen));
        this.leftParen = leftParen;
        this.expr = expr;
        this.rightParen = rightParen;
    }
}
class TagAttribute extends NodeBase {
    constructor(name, equals, expr) {
        if (name && equals && expr) {
            super(mergeRanges(name, expr));
        }
        else {
            super(name.range);
        }
        this.name = name;
        this.equals = equals !== null && equals !== void 0 ? equals : null;
        this.expr = expr !== null && expr !== void 0 ? expr : null;
    }
}
var CfTag;
(function (CfTag) {
    ;
    //
    // end tags are expected to be "common" tags; they should not have attributes or etc.
    //
    class TagBase extends NodeBase {
        constructor(which, tagStart, tagName, voidSlash, tagEnd, canonicalName) {
            super(mergeRanges(tagStart, tagEnd));
            this.which = which;
            this.tagStart = tagStart;
            this.tagName = tagName;
            this.voidSlash = voidSlash;
            this.tagEnd = tagEnd;
            this.canonicalName = canonicalName;
        }
    }
    CfTag.TagBase = TagBase;
    class Common extends TagBase {
        constructor(which, tagStart, tagName, voidSlash, tagEnd, canonicalName, attrs) {
            super(which, tagStart, tagName, voidSlash, tagEnd, canonicalName);
            this.attrs = attrs !== null && attrs !== void 0 ? attrs : new NodeList();
        }
    }
    CfTag.Common = Common;
    class ScriptLike extends TagBase {
        constructor(which, tagStart, tagName, voidSlash, tagEnd, canonicalName, expr) {
            super(which, tagStart, tagName, voidSlash, tagEnd, canonicalName);
            this.expr = expr;
        }
    }
    CfTag.ScriptLike = ScriptLike;
    class Script extends TagBase {
        constructor(tagStart, tagName, voidSlash, tagEnd, canonicalName, stmtList) {
            super(0 /* start */, tagStart, tagName, voidSlash, tagEnd, canonicalName);
            this.stmtList = stmtList;
        }
    }
    CfTag.Script = Script;
    class Text extends TagBase {
        // just interested in the node's range and uniquely identifying it as tag-text
        constructor(range) {
            const nilTerminal = Terminal.Nil();
            super(0 /* start */, nilTerminal, nilTerminal, null, nilTerminal, "");
            this.range = range;
        }
    }
    CfTag.Text = Text;
    class Comment extends TagBase {
        constructor(tagStart, body, tagEnd) {
            const nilTerminal = Terminal.Nil();
            super(0 /* start */, tagStart, nilTerminal, nilTerminal, tagEnd, "");
            this.body = body;
        }
    }
    CfTag.Comment = Comment;
    function assertIsScriptLike(tag) {
        if (tag instanceof ScriptLike)
            return;
        else
            throw "tag was expected to be ScriptLike";
    }
    CfTag.assertIsScriptLike = assertIsScriptLike;
    function assertIsCommon(tag) {
        if (tag instanceof Common)
            return;
        else
            throw "tag was expected to be Common";
    }
    CfTag.assertIsCommon = assertIsCommon;
    function assertIsScript(tag) {
        if (tag instanceof Script)
            return;
        else
            throw "tag was expected to be Script";
    }
    CfTag.assertIsScript = assertIsScript;
})(CfTag = exports.CfTag || (exports.CfTag = {}));
class CallExpression extends NodeBase {
    constructor(identifier, leftParen, args, rightParen) {
        super(mergeRanges(identifier, rightParen));
        this.identifier = identifier;
        this.leftParen = leftParen;
        this.args = args;
        this.rightParen = rightParen;
    }
}
class CallArgument extends NodeBase {
    constructor(expr, comma) {
        super(mergeRanges(expr, comma));
        this.expr = expr;
        this.comma = comma !== null && comma !== void 0 ? comma : null;
    }
}
class Assignment extends NodeBase {
    constructor(finalModifier, varModifier, baseTarget, assignmentChain) {
        if (assignmentChain.length == 0)
            throw "assignment chain must have at least one element";
        super(mergeRanges(finalModifier, varModifier, baseTarget, assignmentChain[assignmentChain.length - 1].value));
        this.finalModifier = finalModifier;
        this.varModifier = varModifier;
        this.baseTarget = baseTarget;
        this.assignmentChain = assignmentChain;
    }
}
;
;
class UnaryOperator extends NodeBase {
    constructor(lexicallyFirst, lexicallyAfter) {
        super(mergeRanges(lexicallyFirst, lexicallyAfter));
        if (lexicallyFirst instanceof Terminal) {
            this.pos = 0 /* pre */;
            this.optype = UnaryOperator.tokenTypeToOpType(lexicallyFirst.token.type);
            this.operator = lexicallyFirst;
            this.expr = lexicallyAfter;
        }
        else {
            this.pos = 1 /* post */;
            this.optype = UnaryOperator.tokenTypeToOpType(lexicallyAfter.token.type);
            this.operator = lexicallyAfter;
            this.expr = lexicallyFirst;
        }
    }
    static tokenTypeToOpType(tokenType) {
        switch (tokenType) {
            case 13 /* DBL_MINUS */: return 1 /* dec */;
            case 15 /* DBL_PLUS */: return 0 /* inc */;
            case 30 /* MINUS */: return 3 /* neg */;
            case 34 /* PLUS */: return 2 /* pos */;
            default: break;
        }
        throw "bad unary op type transform";
    }
}
class BinaryOperator extends NodeBase {
    constructor(left, operator, right) {
        super(mergeRanges(left, right));
        this.left = left;
        this.operator = operator;
        this.right = right;
        this.optype = BinaryOperator.tokenTypeToOpType(operator.token.type);
    }
    static tokenTypeToOpType(tokenType) {
        switch (tokenType) {
            case 34 /* PLUS */: return 0 /* add */;
            case 30 /* MINUS */: return 1 /* sub */;
            case 43 /* STAR */: return 2 /* mul */;
            case 21 /* FORWARD_SLASH */: return 3 /* div */;
            case 32 /* PERCENT */: return 4 /* mod */;
            case 8 /* CARET */: return 5 /* exp */;
            case 6 /* AMPERSAND */: return 6 /* cat */;
            case 11 /* DBL_EQUAL */: return 7 /* eq */;
            case 53 /* LIT_EQ */: return 7 /* eq */;
            case 20 /* EXCLAMATION_EQUAL */: return 8 /* neq */;
            case 64 /* LIT_NEQ */: return 8 /* neq */;
            case 25 /* LEFT_ANGLE */: return 9 /* lt */;
            case 60 /* LIT_LT */: return 9 /* lt */;
            case 26 /* LEFT_ANGLE_EQUAL */: return 10 /* lte */;
            case 62 /* LIT_LTE */: return 10 /* lte */;
            case 61 /* LIT_LE */: return 10 /* lte */;
            case 38 /* RIGHT_ANGLE */: return 11 /* gt */;
            case 54 /* LIT_GT */: return 11 /* gt */;
            case 39 /* RIGHT_ANGLE_EQUAL */: return 12 /* gte */;
            case 55 /* LIT_GTE */: return 12 /* gte */;
            case 56 /* LIT_GE */: return 12 /* gte */;
            default: break;
        }
        throw "bad binary op type transform";
    }
}
;
class Conditional extends NodeBase {
    constructor(subtype, fromTag, consequent) {
        super(fromTag.range);
        this.ifToken = null;
        this.elseToken = null;
        this.leftParen = null;
        this.rightParen = null;
        this.consequent = consequent;
        this.alternative = null;
        this.tagOrigin.startTag = fromTag;
        if (subtype === 0 /* if */ || subtype === 1 /* elseif */) {
            CfTag.assertIsScriptLike(fromTag);
            this.expr = fromTag.expr;
        }
        else /* tag is an else tag */ {
            this.expr = null;
        }
    }
}
class Statement extends NodeBase {
    constructor(tag) {
        super(tag.range);
        CfTag.assertIsCommon(tag);
        this.stmt = null;
        this.tagOrigin.startTag = tag;
        this.semicolon = null;
        //
        // will probably need to determine which of the "cf-built-in" statements this is;
        // or maybe the caller will have to do that, and constructing from "any start tag" doesn't make sense
        //
    }
}
class NamedBlock extends NodeBase {
    constructor(startTag, endTagOrStmtList, endTag) {
        if (endTag) {
            super(mergeRanges(startTag, endTag));
            this.tagOrigin.startTag = startTag;
            this.tagOrigin.endTag = endTag;
            this.name = null;
            this.attrs = null;
            this.leftBrace = null;
            this.stmtList = endTagOrStmtList;
            this.rightBrace = null;
        }
        else {
            super(mergeRanges(startTag, endTagOrStmtList));
            this.tagOrigin.startTag = startTag;
            this.tagOrigin.endTag = endTagOrStmtList;
            this.name = null;
            this.attrs = null;
            this.leftBrace = null;
            this.stmtList = new NodeList();
            this.rightBrace = null;
        }
    }
}
class StringLiteral extends NodeBase {
    constructor(arg0, leftQuote, elements, rightQuote) {
        if (arg0 instanceof Terminal) {
            super(arg0.range);
            this.delimiter = null;
            this.leftQuote = null;
            this.elements = new NodeList([arg0]);
            this.rightQuote = null;
        }
        else {
            super(mergeRanges(leftQuote, rightQuote));
            this.delimiter = arg0;
            this.leftQuote = leftQuote;
            this.elements = elements;
            this.rightQuote = rightQuote;
        }
    }
}
class NumericLiteral extends NodeBase {
    constructor(literal) {
        super(literal.range);
        this.literal = literal;
    }
}
class Identifier extends NodeBase {
    constructor(identifier, name) {
        super(identifier.range);
        this.identifier = identifier;
        this.canonicalName = name.toLowerCase();
    }
}
class IndexedAccess extends NodeBase {
    constructor(root) {
        super(root.range);
        this.accessElements = [];
        this.root = root;
    }
    pushAccessElement(dotOrBracket, expr, rightBracket) {
        if (rightBracket) { // bracket access
            this.accessElements.push({
                leftBracket: dotOrBracket,
                expr: expr,
                rightBracket: rightBracket
            });
        }
        else { // dot access
            this.accessElements.push({
                dot: dotOrBracket,
                propertyName: expr
            });
        }
    }
}
;
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
    };
}
function Parser(tokenizer_, mode_ = 0 /* tag */) {
    let tokenizer = tokenizer_;
    let mode = mode_;
    let lookahead_ = peek().type;
    let globalDiagnosticMsg = null;
    const diagnostics = [];
    let parseErrorBeforeNextFinishedNode = false;
    parseErrorBeforeNextFinishedNode;
    function peek(jump = 0) {
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
    function getTokenizerState() {
        return {
            index: tokenizer.getIndex(),
            mode: mode,
            artificialEndLimit: tokenizer.getArtificalEndLimit()
        };
    }
    function restoreTokenizerState(state) {
        tokenizer.restoreIndex(state.index);
        mode = state.mode;
        if (state.artificialEndLimit) {
            tokenizer.setArtificialEndLimit(state.artificialEndLimit);
        }
        else {
            tokenizer.clearArtificalEndLimit();
        }
    }
    function tagMode() {
        return mode === 0 /* tag */;
    }
    function parseErrorAtRange(fromInclusiveOrRange, toExclusiveOrMsg, msg) {
        parseErrorBeforeNextFinishedNode = true;
        const diagnosticMsg = typeof msg === "string"
            ? msg
            : typeof toExclusiveOrMsg === "string"
                ? toExclusiveOrMsg
                : undefined;
        diagnostics.push({
            fileName: "<NYI>",
            fromInclusive: typeof fromInclusiveOrRange === "number"
                ? fromInclusiveOrRange
                : fromInclusiveOrRange.fromInclusive,
            toExclusive: typeof toExclusiveOrMsg === "number"
                ? toExclusiveOrMsg
                : fromInclusiveOrRange.toExclusive,
            msg: diagnosticMsg !== null && diagnosticMsg !== void 0 ? diagnosticMsg : globalDiagnosticMsg
        });
    }
    function parseErrorAtCurrentToken(msg) {
        const sourceRange = peek().range;
        parseErrorAtRange(sourceRange.fromInclusive, sourceRange.toExclusive, msg);
    }
    function createMissingNode(node) {
        node.flags |= 1 /* error */ | 2 /* missing */;
        return node;
    }
    function parseOptionalTerminal(type, parseOptions) {
        if (lookahead() === type) {
            const token = next();
            if (parseOptions & 1 /* withTrivia */) {
                return new Terminal(token, parseTrivia());
            }
            else {
                return new Terminal(token);
            }
        }
        else {
            return null;
        }
    }
    function parseExpectedTerminal(type, parseOptions) {
        const maybeTerminal = parseOptionalTerminal(type, parseOptions);
        if (maybeTerminal) {
            return maybeTerminal;
        }
        else {
            parseErrorAtCurrentToken(globalDiagnosticMsg !== null && globalDiagnosticMsg !== void 0 ? globalDiagnosticMsg : "Expected a <??> here");
            const phonyToken = new tokenizer_1.Token(type, scanner_1.SourceRange.Nil());
            return createMissingNode(new Terminal(phonyToken));
        }
    }
    function parseTagComment() {
        const commentStart = parseExpectedTerminal(48 /* CF_TAG_COMMENT_START */, 0 /* noTrivia */);
        const commentBody = parseTagTrivia();
        const commentEnd = parseExpectedTerminal(49 /* CF_TAG_COMMENT_END */, 0 /* noTrivia */);
        // if no comment end, emit an error
        return new CfTag.Comment(commentStart, commentBody, commentEnd);
    }
    function parseScriptSingleLineComment() {
        throw "nyi";
    }
    function parseScriptMultiLineComment() {
        throw "nyi";
    }
    function parseTrivia() {
        if (tagMode()) {
            return parseTagTrivia();
        }
        const result = new NodeList();
        while (true) {
            switch (lookahead()) {
                case 12 /* DBL_FORWARD_SLASH */:
                    result.list.push(parseScriptSingleLineComment());
                    continue;
                case 23 /* FORWARD_SLASH_STAR */:
                    result.list.push(parseScriptMultiLineComment());
                    continue;
                case 3 /* WHITESPACE */:
                    result.list.push(new TextSpan(next().range));
                    continue;
            }
            break;
        }
        return result;
    }
    function parseTagTrivia() {
        const result = new NodeList();
        while (true) {
            switch (lookahead()) {
                case 48 /* CF_TAG_COMMENT_START */: {
                    result.list.push(parseTagComment());
                    continue;
                }
                case 3 /* WHITESPACE */: {
                    result.list.push(new CfTag.Text(next().range));
                    continue;
                }
            }
            // if we didn't match tag comment start or whitespace, we're done
            break;
        }
        return result;
    }
    function parseCfStartTag() {
        const tagStart = parseExpectedTerminal(45 /* CF_START_TAG_START */, 0 /* noTrivia */);
        const tagName = parseExpectedTerminal(4 /* LEXEME */, 1 /* withTrivia */);
        const canonicalName = tokenizer.getTokenText(tagName.token).toLowerCase();
        if (canonicalName === "if" || canonicalName === "elseif") {
            const expr = parseExpression();
            const rightAngle = parseExpectedTerminal(38 /* RIGHT_ANGLE */, 0 /* noTrivia */);
            return new CfTag.ScriptLike(0 /* start */, tagStart, tagName, null, rightAngle, canonicalName, expr);
        }
        else if (canonicalName === "set") {
            const expr = parseAssignmentOrLower();
            const rightAngle = parseExpectedTerminal(38 /* RIGHT_ANGLE */, 0 /* noTrivia */);
            return new CfTag.ScriptLike(0 /* start */, tagStart, tagName, null, rightAngle, canonicalName, expr);
        }
        else {
            const tagAttrs = parseTagAttributes();
            const maybeVoidSlash = parseOptionalTerminal(21 /* FORWARD_SLASH */, 1 /* withTrivia */);
            const rightAngle = parseExpectedTerminal(38 /* RIGHT_ANGLE */, 1 /* withTrivia */);
            return new CfTag.Common(0 /* start */, tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, tagAttrs);
        }
    }
    function parseCfEndTag() {
        const tagStart = parseExpectedTerminal(46 /* CF_END_TAG_START */, 0 /* noTrivia */);
        const tagName = parseExpectedTerminal(4 /* LEXEME */, 1 /* withTrivia */);
        const rightAngle = parseExpectedTerminal(38 /* RIGHT_ANGLE */, 0 /* noTrivia */);
        let canonicalName = "";
        if (!(tagName.flags & 2 /* missing */)) {
            canonicalName = tokenizer.getTokenText(tagName.token).toLowerCase();
        }
        return new CfTag.Common(1 /* end */, tagStart, tagName, null, rightAngle, canonicalName);
    }
    function parseTagAttributes() {
        const result = new NodeList();
        while (lookahead() === 4 /* LEXEME */) {
            const attrName = parseExpectedTerminal(4 /* LEXEME */, 1 /* withTrivia */);
            if (lookahead() === 17 /* EQUAL */) {
                const equal = parseExpectedTerminal(17 /* EQUAL */, 1 /* withTrivia */);
                let value;
                if (lookahead() === 4 /* LEXEME */) {
                    value = parseStringLiteral(4 /* undelimitedString */);
                }
                else {
                    value = parseExpression();
                }
                result.list.push(new TagAttribute(attrName, equal, value));
            }
            else {
                result.list.push(new TagAttribute(attrName));
            }
        }
        return result;
    }
    function parseTags() {
        //
        // tag treeifier
        // after parsing tags, we end up with just a list of tags
        // we need to go through a second time and turn them into a tree
        // we convert text spans into interpolated text spans here if necessary
        // as well as convert all tags into either blocks or statements;
        // a tag with children becomes a block, a single tag (like a <cfset ...> or just a loose <cffile ...> ) becomes a statement
        // this also gives us the opportunity to emit diagnostics for unbalanced start or end tags
        //
        function treeifyTagList(tagList) {
            const tagContext = TagContext();
            let index = 0;
            ;
            const reductionInstructions = {
                cfif: [
                    { onHitWhich: 0 /* start */, onHitName: "elseif", reduction: 1 /* return */ },
                    { onHitWhich: 0 /* start */, onHitName: "else", reduction: 1 /* return */ },
                    { onHitWhich: 1 /* end */, onHitName: "if", reduction: 1 /* return */ }
                ],
                cfelseif: [
                    { onHitWhich: 0 /* start */, onHitName: "elseif", reduction: 1 /* return */ },
                    { onHitWhich: 0 /* start */, onHitName: "else", reduction: 1 /* return */ },
                    { onHitWhich: 1 /* end */, onHitName: "if", reduction: 1 /* return */ }
                ],
                cfelse: [
                    { onHitWhich: 1 /* end */, onHitName: "if", reduction: 1 /* return */ }
                ],
                default: []
            };
            function updateTagContext(tag) {
                let bumpDir = tag.which === 0 /* start */ ? 1 : -1;
                switch (tag.canonicalName) {
                    case "output":
                    case "mail":
                    case "query": {
                        tagContext.depth[tag.canonicalName] += bumpDir;
                    }
                }
            }
            function stopAt(which, name, reduction) {
                return [{
                        onHitWhich: which,
                        onHitName: name,
                        reduction: reduction
                    }];
            }
            function getReductionScheme(tag, instructions) {
                for (const instr of instructions) {
                    if (instr.onHitWhich === tag.which && instr.onHitName === tag.canonicalName) {
                        return instr.reduction;
                    }
                }
                return 0 /* default */;
            }
            function hasNextTag() {
                return index < tagList.list.length;
            }
            function nextTag() {
                return tagList.list[index++];
            }
            function peekTag() {
                return tagList.list[index];
            }
            function parseOptionalTag(which, canonicalName) {
                if (hasNextTag() && tagList.list[index].which === which && tagList.list[index].canonicalName === canonicalName) {
                    return nextTag();
                }
                else {
                    return null;
                }
            }
            function parseExpectedTag(which, canonicalName) {
                const tag = parseOptionalTag(which, canonicalName);
                if (tag) {
                    return tag;
                }
                else {
                    // emit diagnostic
                    const msg = `Expected a <${which === 1 /* end */ ? "/" : ""}${canonicalName}> tag here`;
                    let range = hasNextTag() ? peekTag().range : tagList.list[tagList.list.length - 1].range;
                    parseErrorAtRange(range, msg);
                    // create fake placeholder tag
                    let missingTag;
                    if (which === 0 /* start */) {
                        missingTag = new CfTag.Common(which, Terminal.Nil(), Terminal.Nil(), null, Terminal.Nil(), canonicalName, new NodeList());
                    }
                    else {
                        missingTag = new CfTag.Common(which, Terminal.Nil(), Terminal.Nil(), null, Terminal.Nil(), canonicalName);
                    }
                    createMissingNode(missingTag);
                    return missingTag;
                }
            }
            function treeifyConditionalTag() {
                const ifTag = parseExpectedTag(0 /* start */, "if");
                const rootConsequent = treeifyTags(reductionInstructions.cfif);
                let root = new Conditional(0 /* if */, ifTag, rootConsequent);
                let working = root;
                while (true) {
                    const elseIfTag = parseOptionalTag(0 /* start */, "elseif");
                    if (elseIfTag) {
                        const consequent = treeifyTags(reductionInstructions.cfelseif);
                        working.alternative = new Conditional(1 /* elseif */, elseIfTag, consequent);
                        working = root.alternative;
                        continue;
                    }
                    const elseTag = parseOptionalTag(0 /* start */, "else");
                    if (elseTag) {
                        const consequent = treeifyTags(reductionInstructions.cfelse);
                        working.alternative = new Conditional(2 /* else */, elseTag, consequent);
                    }
                    break;
                }
                root.tagOrigin.endTag = parseExpectedTag(1 /* end */, "if");
                return root;
            }
            function treeifyTags(reductionInstructions) {
                const result = new NodeList();
                // any CfTags left in the result, which were in there to act as anchors that end tags could match against,
                // get turned into statements
                // all text and comments by now should have been turned into TextSpans
                // is returning an r-value reasonable? goal is to use this as the return statement, like `return finalize();`
                function finalize(from = 0) {
                    for (let i = from; i < result.list.length; i++) {
                        if (result.list[i] instanceof CfTag.TagBase) {
                            result.list[i] = new Statement(result.list[i]);
                        }
                    }
                    return result;
                }
                function localStackFindMatchingStartTag(tag) {
                    for (let i = result.list.length - 1; i >= 0; i--) {
                        if (result.list[i] instanceof CfTag.TagBase) {
                            const stackTag = result.list[i];
                            if (stackTag.which === 1 /* end */ && stackTag.canonicalName === tag.canonicalName) {
                                return i;
                            }
                        }
                    }
                    return null;
                }
                while (hasNextTag()) {
                    const tag = peekTag();
                    updateTagContext(tag);
                    if (tag instanceof CfTag.Text) {
                        parseTextInTagContext(tag.range, tagContext);
                        continue;
                    }
                    else if (tag.which === 0 /* start */) {
                        const reductionScheme = getReductionScheme(tag, reductionInstructions);
                        switch (reductionScheme) {
                            case 1 /* return */:
                                return finalize();
                            case 0 /* default */:
                                // ok, no-op: the default reduction scheme for a start tag is to do nothing
                                break;
                        }
                        if (tag.canonicalName === "if") {
                            result.list.push(treeifyConditionalTag());
                            continue;
                        }
                        else if (tag.canonicalName === "set") {
                            CfTag.assertIsScriptLike(tag);
                            const expr = tag.expr;
                            expr.tagOrigin.startTag = tag;
                            result.list.push(expr);
                            nextTag();
                            continue;
                        }
                        else if (tag.canonicalName === "script") {
                            // same as handling set, except there is an end tag to consume
                            CfTag.assertIsScript(tag);
                            const stmtList = tag.stmtList;
                            stmtList.tagOrigin.startTag = tag;
                            nextTag();
                            stmtList.tagOrigin.endTag = parseExpectedTag(1 /* end */, "script");
                            // so our result now has at least one list in it, looking something like (node | node[])[]
                            result.list.push(stmtList);
                            continue;
                        }
                        else {
                            if (requiresEndTag(tag)) {
                                const startTag = tag;
                                nextTag();
                                const blockChildren = treeifyTags(stopAt(1 /* end */, startTag.canonicalName, 0 /* default */));
                                if (!hasNextTag()) {
                                    parseErrorAtCurrentToken("no matching end tag for " + tag.canonicalName);
                                    throw "nyi";
                                }
                                else {
                                    const endTag = parseExpectedTag(1 /* end */, startTag.canonicalName);
                                    updateTagContext(endTag);
                                    result.list.push(new NamedBlock(startTag, blockChildren, endTag));
                                }
                            }
                            else {
                                // a single loose tag
                                // it will become a statement
                                // e.g., <cfhttp args /> is essentially a call to a fictitious "cfhttp(args)" function, except it is a statement instead of a value producing expression
                                // but first we push it into the result as a tag, so that any possible matching end tags can walk up the local results list
                                // and find it 
                                result.list.push(tag);
                                nextTag();
                            }
                        }
                    }
                    else if (tag.which === 1 /* end */) {
                        const reductionScheme = getReductionScheme(tag, reductionInstructions);
                        if (reductionScheme === 1 /* return */) {
                            return finalize();
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
                            const matchingStartTag = result.list[matchingStartTagIndex];
                            //
                            // finalize so any remaining loose tag bodies are statement-ized
                            // move (matching_tag + 0) to be the head of a new block
                            // move everything from (matching_tag+1) into the new block as children
                            // use current tag as end of tag block
                            // once everything has been moved into their new homes, drop the original references
                            // from our local results list
                            //
                            finalize(matchingStartTagIndex + 1);
                            const blockChildren = new NodeList(result.list.slice(matchingStartTagIndex + 1));
                            result.list.splice(matchingStartTagIndex);
                            result.list.push(new NamedBlock(matchingStartTag, blockChildren, tag));
                        }
                        else {
                            parseErrorAtRange(tag.range, "End tag without a matching start tag (cf" + tag.canonicalName + ")");
                        }
                        nextTag();
                    }
                }
                return finalize();
            }
            return treeifyTags(reductionInstructions.default);
        }
        const saveMode = mode;
        mode = 0 /* tag */;
        const result = new NodeList();
        let tagTextRange = scanner_1.SourceRange.Nil();
        function startOrContinueTagTextRange() {
            if (tagTextRange.isNil()) {
                const index = tokenizer.getIndex();
                tagTextRange = new scanner_1.SourceRange(index, index + 1);
            }
        }
        function finishTagTextRange() {
            if (tagTextRange.isNil()) {
                return;
            }
            tagTextRange.toExclusive = tokenizer.getIndex() + 1;
            result.list.push(new CfTag.Text(tagTextRange));
            tagTextRange = scanner_1.SourceRange.Nil();
        }
        while (lookahead_ != 1 /* EOF */) {
            switch (lookahead_) {
                case 45 /* CF_START_TAG_START */: {
                    finishTagTextRange();
                    const tag = parseCfStartTag();
                    if (tag.canonicalName === "script") {
                        // parse cfscript ...
                    }
                    else {
                        result.list.push(tag);
                    }
                    break;
                }
                case 46 /* CF_END_TAG_START */: {
                    finishTagTextRange();
                    result.list.push(parseCfEndTag());
                    break;
                }
                case 48 /* CF_TAG_COMMENT_START */: {
                    finishTagTextRange();
                    result.list.push(parseTagComment());
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
    function parseTextInTagContext(range, tagContext) {
        if (!tagContext.inTextInterpolationContext()) {
            return new TextSpan(range);
        }
        const saveTokenizerState = getTokenizerState();
        restoreTokenizerState({
            index: range.fromInclusive,
            mode: 0 /* tag */,
            artificialEndLimit: range.toExclusive
        });
        const result = parseTextWithPossibleInterpolations();
        restoreTokenizerState(saveTokenizerState);
        return result;
    }
    function parseTextWithPossibleInterpolations(quoteDelimiter) {
        const result = new NodeList();
        let textSourceRange = scanner_1.SourceRange.Nil();
        function startOrContinueTextRange() {
            if (textSourceRange.isNil()) {
                const index = tokenizer.getIndex();
                textSourceRange = new scanner_1.SourceRange(index, index + 1);
            }
            // continuing is a no-op; when we finish the text range, we'll update the "toExclusive"
            // with the tokenizer's current index
        }
        function finishTextRange() {
            // if we hadn't started a range yet, we're done
            if (textSourceRange.isNil()) {
                return;
            }
            textSourceRange.toExclusive = tokenizer.getIndex() + 1;
            result.list.push(new TextSpan(textSourceRange));
            textSourceRange = scanner_1.SourceRange.Nil();
        }
        //
        // there is no anchor to stop at other than EOF if the delimiter undefined
        // we rely on the caller setting the tokenizer to have an artifical range limit, such that we
        // eat only some pre-determined range of what we consider to be valid text
        //
        while (lookahead() != 1 /* EOF */) {
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
                case 24 /* HASH */: {
                    if (peek(1).type === 24 /* HASH */) { // doubled up hash, meaning it is an escaped hash symbol
                        startOrContinueTextRange();
                        next(), next();
                        continue;
                    }
                    else { // single hash, meaning this is an interpolated string element
                        finishTextRange();
                        const leftHash = parseExpectedTerminal(24 /* HASH */, 1 /* withTrivia */);
                        const expr = parseExpression();
                        const rightHash = parseExpectedTerminal(24 /* HASH */, 1 /* withTrivia */);
                        result.list.push(new HashWrappedExpr(leftHash, expr, rightHash));
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
    function isAssignmentTarget(node) {
        switch (node.constructor) {
            case IndexedAccess:
            case Identifier:
                return true;
            case HashWrappedExpr:
                return isAssignmentTarget(node.expr);
            default:
                return false;
        }
    }
    function parseAssignmentOrLower() {
        const finalModifier = parseOptionalTerminal(73 /* KW_FINAL */, 1 /* withTrivia */);
        const varModifier = parseOptionalTerminal(81 /* KW_VAR */, 1 /* withTrivia */);
        const root = parseCallExpressionOrLower();
        if (lookahead() != 17 /* EQUAL */) {
            return root;
        }
        if (!isAssignmentTarget(root)) {
            // if we had a final or var modifer we need to mark an error, but this is otherwise OK
            // "left-hand side of an assignment must be an assignment target"
            return root;
        }
        const assignmentChain = [];
        do {
            assignmentChain.push({
                equals: parseExpectedTerminal(17 /* EQUAL */, 1 /* withTrivia */),
                value: parseExpression()
            });
        } while (lookahead() === 17 /* EQUAL */ && isAssignmentTarget(assignmentChain[assignmentChain.length - 1].value));
        return new Assignment(finalModifier, varModifier, root, assignmentChain);
    }
    function parseExpression() {
        let root = parseBooleanExpression();
        while (true) {
            if (tagMode() && lookahead() === 25 /* LEFT_ANGLE */ || lookahead() === 38 /* RIGHT_ANGLE */) {
                break;
            }
            switch (lookahead()) {
                case 11 /* DBL_EQUAL */: // [[fallthrough]];
                case 53 /* LIT_EQ */: // [[fallthrough]];
                case 20 /* EXCLAMATION_EQUAL */: // [[fallthrough]];
                case 64 /* LIT_NEQ */: // [[fallthrough]];
                case 25 /* LEFT_ANGLE */: // [[fallthrough]]; // invalid in tag mode, but we've already checked for it
                case 60 /* LIT_LT */: // [[fallthrough]];
                case 26 /* LEFT_ANGLE_EQUAL */: // [[fallthrough]];
                case 62 /* LIT_LTE */: // [[fallthrough]];
                case 61 /* LIT_LE */: // [[fallthrough]];
                case 38 /* RIGHT_ANGLE */: // [[fallthrough]]; // invalid in tag mode, but we've already checked for it
                case 54 /* LIT_GT */: // [[fallthrough]];
                case 39 /* RIGHT_ANGLE_EQUAL */: // [[fallthrough]];
                case 55 /* LIT_GTE */: // [[fallthrough]];
                case 56 /* LIT_GE */: {
                    const op = parseExpectedTerminal(lookahead(), 1 /* withTrivia */);
                    const right = parseExpression();
                    root = new BinaryOperator(root, op, right);
                    continue;
                }
            }
            // if we didn't match any of the above tokens, we're done
            break;
        }
        return root;
    }
    function parseBooleanExpression(descendIntoOr = true) {
        let root = parseAddition();
        outer: while (true) {
            switch (lookahead()) {
                case 14 /* DBL_PIPE */:
                case 65 /* LIT_OR */:
                case 66 /* LIT_XOR */: {
                    if (!descendIntoOr)
                        break outer;
                    const op = parseExpectedTerminal(lookahead(), 1 /* withTrivia */);
                    const expr = parseAddition();
                    root = new BinaryOperator(root, op, expr);
                    continue;
                }
                case 10 /* DBL_AMPERSAND */:
                case 50 /* LIT_AND */: {
                    const op = parseExpectedTerminal(lookahead(), 1 /* withTrivia */);
                    const expr = parseBooleanExpression(/*descendIntoOr*/ false);
                    root = new BinaryOperator(root, op, expr);
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
                case 34 /* PLUS */:
                case 30 /* MINUS */:
                case 6 /* AMPERSAND */: {
                    const op = parseExpectedTerminal(lookahead(), 1 /* withTrivia */);
                    const expr = parseMultiplication();
                    root = new BinaryOperator(root, op, expr);
                    continue;
                }
            }
            // if we didn't match any of the above operators, we're done
            break;
        }
        return root;
    }
    function parseMultiplication() {
        const stack = [];
        // bind "^" (exponentiation) right
        function reduceRight() {
            if (stack.length === 1)
                return;
            // the stack should always have an odd number of elements,
            // (expr (op expr (op expr ...) ...) ...)
            while (stack.length > 1 && stack[stack.length - 2].token.type === 8 /* CARET */) {
                const reduced = new BinaryOperator(stack[stack.length - 3], stack[stack.length - 2], stack[stack.length - 1]);
                stack.splice(stack.length - 3, 3);
                stack.push(reduced);
            }
        }
        stack.push(parseParentheticalOrUnaryPrefix());
        while (true) {
            switch (lookahead()) {
                case 43 /* STAR */:
                case 21 /* FORWARD_SLASH */: {
                    reduceRight();
                    const op = parseExpectedTerminal(lookahead(), 1 /* withTrivia */);
                    const expr = parseParentheticalOrUnaryPrefix();
                    stack.push(op, expr);
                    continue;
                }
                case 8 /* CARET */: {
                    const op = parseExpectedTerminal(lookahead(), 1 /* withTrivia */);
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
            let result = new BinaryOperator(stack[0], stack[1], stack[2]);
            for (let i = 3; i < stack.length; i += 2) {
                result = new BinaryOperator(result, stack[i], stack[i + 1]);
                i += 2;
            }
            return result;
        }
    }
    function parseParentheticalOrUnaryPrefix() {
        switch (lookahead()) {
            case 29 /* LEFT_PAREN */: {
                const leftParen = parseExpectedTerminal(29 /* LEFT_PAREN */, 1 /* withTrivia */);
                const expr = parseExpression();
                const rightParen = parseExpectedTerminal(42 /* RIGHT_PAREN */, 1 /* withTrivia */);
                return new Parenthetical(leftParen, expr, rightParen);
            }
            case 13 /* DBL_MINUS */: // [[fallthrough]];
            case 15 /* DBL_PLUS */: {
                const op = parseExpectedTerminal(lookahead(), 1 /* withTrivia */);
                const expr = parseCallExpressionOrLower();
                return new UnaryOperator(op, expr);
            }
            default: return parseCallExpressionOrLower();
        }
    }
    function parseCallExpressionOrLower() {
        switch (lookahead()) {
            case 5 /* NUMBER */: return parseNumericLiteral();
            case 37 /* QUOTE_DOUBLE */: // fallthrough
            case 36 /* QUOTE_SINGLE */: return parseStringLiteral();
            default: break;
        }
        let root = parseIdentifier(2 /* allowHashWrapped */);
        if (root instanceof HashWrappedExpr) {
            return root;
        }
        while (lookahead() != 1 /* EOF */) {
            switch (lookahead()) {
                case 27 /* LEFT_BRACE */:
                case 16 /* DOT */:
                    root = parseIndexedAccess(root);
                    continue;
                case 29 /* LEFT_PAREN */:
                    root = parseCallExpression(root);
                    continue;
            }
            break;
        }
        if (root instanceof CallExpression) {
            switch (lookahead()) {
                case 15 /* DBL_PLUS */:
                case 13 /* DBL_MINUS */:
                    const unaryOp = parseExpectedTerminal(lookahead(), 1 /* withTrivia */);
                    root = new UnaryOperator(root, unaryOp);
            }
        }
        return root;
    }
    function parseIndexedAccess(root) {
        while (lookahead() != 1 /* EOF */) {
            switch (lookahead()) {
                case 28 /* LEFT_BRACKET */: {
                    const leftBracket = parseExpectedTerminal(28 /* LEFT_BRACKET */, 1 /* withTrivia */);
                    const expr = parseExpression();
                    const rightBracket = parseExpectedTerminal(41 /* RIGHT_BRACKET */, 1 /* withTrivia */);
                    if (!(root instanceof IndexedAccess)) {
                        root = new IndexedAccess(root);
                    }
                    root.pushAccessElement(leftBracket, expr, rightBracket);
                    continue;
                }
                case 16 /* DOT */: {
                    const dot = parseExpectedTerminal(16 /* DOT */, 1 /* withTrivia */);
                    const propertyName = parseStringLiteral(4 /* undelimitedString */);
                    if (!(root instanceof IndexedAccess)) {
                        root = new IndexedAccess(root);
                    }
                    root.pushAccessElement(dot, propertyName);
                    continue;
                }
            }
            break;
        }
        return root;
    }
    function parseCallExpression(root) {
        const leftParen = parseExpectedTerminal(29 /* LEFT_PAREN */, 1 /* withTrivia */);
        const args = new NodeList();
        while (lookahead() != 1 /* EOF */ && lookahead() != 42 /* RIGHT_PAREN */) {
            const expr = parseExpression();
            const comma = parseOptionalTerminal(9 /* COMMA */, 1 /* withTrivia */);
            args.list.push(new CallArgument(expr, comma));
        }
        const rightParen = parseExpectedTerminal(42 /* RIGHT_PAREN */, 1 /* withTrivia */);
        return new CallExpression(root, leftParen, args, rightParen);
    }
    function parseIdentifier(parseOptions) {
        let leftHash = null;
        if (parseOptions & 2 /* allowHashWrapped */) {
            leftHash = parseOptionalTerminal(24 /* HASH */, 1 /* withTrivia */);
        }
        const identifier = parseExpectedTerminal(4 /* LEXEME */, 1 /* withTrivia */);
        const name = tokenizer.getTokenText(identifier.token);
        if (parseOptions & 2 /* allowHashWrapped */ && leftHash) {
            const rightHash = parseExpectedTerminal(24 /* HASH */, 1 /* withTrivia */);
            return new HashWrappedExpr(leftHash, new Identifier(identifier, name), rightHash);
        }
        else {
            return new Identifier(identifier, name);
        }
    }
    function parseStringLiteral(parseOptions = 0 /* none */) {
        if (parseOptions & 4 /* undelimitedString */) { // e.g., 'foo' in `<cfwhatever attr=foo>`
            const unquotedString = parseExpectedTerminal(4 /* LEXEME */, 1 /* withTrivia */);
            return new StringLiteral(unquotedString);
        }
        else {
            const quoteType = lookahead();
            if (quoteType !== 36 /* QUOTE_SINGLE */ && quoteType !== 37 /* QUOTE_DOUBLE */) {
                // will a lookahead or speculate ever trigger this ... ?
                throw "AssertionFailure: parseStringLiteral called on input without valid string delimiter";
            }
            const leftQuote = parseExpectedTerminal(quoteType, 0 /* noTrivia */);
            const stringElements = parseTextWithPossibleInterpolations();
            const rightQuote = parseExpectedTerminal(quoteType, 1 /* withTrivia */);
            return new StringLiteral(quoteType, leftQuote, stringElements, rightQuote);
        }
    }
    function parseNumericLiteral() {
        return new NumericLiteral(parseExpectedTerminal(5 /* NUMBER */, 1 /* withTrivia */));
    }
    function getDiagnostics() {
        return diagnostics;
    }
    return {
        parseTags,
        getDiagnostics
    };
}
exports.Parser = Parser;
//# sourceMappingURL=parser.js.map

/***/ }),

/***/ "./out/compiler/scanner.js":
/*!*********************************!*\
  !*** ./out/compiler/scanner.js ***!
  \*********************************/
/***/ ((__unused_webpack_module, exports) => {

"use strict";

Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Scanner = exports.SourceRange = void 0;
//
// @fixme -- instead of a 'Nil' token; just use null; but this requires some wide reaching changes,
// and we'd rather not have to write `range!` everywhere to declare that it is non-null, so a design that guarantees a source range is not null
// from within the type system would be nice
//
class SourceRange {
    constructor(fromInclusive, toExclusive) {
        this.fromInclusive = fromInclusive;
        this.toExclusive = toExclusive;
    }
    static Nil() {
        return new SourceRange(-1, -1);
    }
    isNil() {
        return this.fromInclusive == -1 && this.toExclusive == -1;
    }
}
exports.SourceRange = SourceRange;
function Scanner(sourceText_) {
    const sourceText = sourceText_;
    const annotatedChars = annotate(sourceText);
    let end = annotatedChars.length;
    let index = 0;
    //
    // @fixme:
    // will work for ascii or utf16, but multibyte utf8 will probably break
    //
    function annotate(text) {
        let index = 0;
        let col = 0;
        let line = 0;
        const result = [];
        for (const c of text) {
            result.push({
                codepoint: c.charCodeAt(0),
                index,
                col,
                line
            });
            if (c == '\n') {
                line += 1;
                col = 0;
            }
            else {
                col += 1;
            }
            index += 1;
        }
        // push "eof"
        result.push({
            codepoint: 0,
            index,
            col,
            line
        });
        return result;
    }
    function getIndex() {
        return index;
    }
    function restoreIndex(restoreIndex) {
        index = restoreIndex;
    }
    function getArtificalEndLimit() {
        return end;
    }
    function setArtificialEndLimit(offset) {
        end = offset;
    }
    function clearArtificalEndLimit() {
        end = annotatedChars.length;
    }
    function hasNext(jump = 0) {
        return index + jump < end;
    }
    function peek(jump = 0) {
        if (hasNext(jump)) {
            return annotatedChars[index + jump];
        }
        return null;
    }
    function next() {
        if (!hasNext()) {
            throw "scanner : call to next after EOF";
        }
        return annotatedChars[index++];
    }
    function maybeEat(pattern) {
        pattern.lastIndex = index;
        const match = pattern.exec(sourceText);
        if (match) {
            index += match[0].length;
            return true;
        }
        else {
            return false;
        }
    }
    return {
        getIndex,
        peek,
        hasNext,
        next,
        setArtificialEndLimit,
        getArtificalEndLimit,
        clearArtificalEndLimit,
        restoreIndex,
        maybeEat,
    };
}
exports.Scanner = Scanner;
//# sourceMappingURL=scanner.js.map

/***/ }),

/***/ "./out/compiler/tokenizer.js":
/*!***********************************!*\
  !*** ./out/compiler/tokenizer.js ***!
  \***********************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Tokenizer = exports.Token = exports.TokenTypeUiString = void 0;
const scanner_1 = __webpack_require__(/*! ./scanner */ "./out/compiler/scanner.js");
;
exports.TokenTypeUiString = {
    [0 /* NIL */]: "nil",
    [1 /* EOF */]: "eof",
    [2 /* BOM_UTF_8 */]: "utf8-bom",
    [3 /* WHITESPACE */]: "<whitespace>",
    [4 /* LEXEME */]: "<lexeme>",
    [5 /* NUMBER */]: "<number>",
    [6 /* AMPERSAND */]: "&",
    [7 /* AMPERSAND_EQUAL */]: "&=",
    [8 /* CARET */]: "^",
    [9 /* COMMA */]: ",",
    [10 /* DBL_AMPERSAND */]: "&&",
    [11 /* DBL_EQUAL */]: "==",
    [12 /* DBL_FORWARD_SLASH */]: "//",
    [13 /* DBL_MINUS */]: "--",
    [14 /* DBL_PIPE */]: "||",
    [15 /* DBL_PLUS */]: "++",
    [16 /* DOT */]: ".",
    [17 /* EQUAL */]: "=",
    [18 /* EQUAL_RIGHT_ANGLE */]: "=>",
    [19 /* EXCLAMATION */]: "!",
    [20 /* EXCLAMATION_EQUAL */]: "!=",
    [21 /* FORWARD_SLASH */]: "/",
    [22 /* FORWARD_SLASH_EQUAL */]: "/=",
    [23 /* FORWARD_SLASH_STAR */]: "/*",
    [24 /* HASH */]: "#",
    [25 /* LEFT_ANGLE */]: "<",
    [26 /* LEFT_ANGLE_EQUAL */]: "<=",
    [27 /* LEFT_BRACE */]: "{",
    [28 /* LEFT_BRACKET */]: "[",
    [29 /* LEFT_PAREN */]: "(",
    [30 /* MINUS */]: "-",
    [31 /* MINUS_EQUAL */]: "-=",
    [32 /* PERCENT */]: "%",
    [33 /* PIPE */]: "|",
    [34 /* PLUS */]: "+",
    [35 /* PLUS_EQUAL */]: "+=",
    [36 /* QUOTE_SINGLE */]: "'",
    [37 /* QUOTE_DOUBLE */]: "\"",
    [38 /* RIGHT_ANGLE */]: ">",
    [39 /* RIGHT_ANGLE_EQUAL */]: ">=",
    [40 /* RIGHT_BRACE */]: "}",
    [41 /* RIGHT_BRACKET */]: "]",
    [42 /* RIGHT_PAREN */]: ")",
    [43 /* STAR */]: "*",
    [44 /* STAR_EQUAL */]: "*=",
    [45 /* CF_START_TAG_START */]: "<cf",
    [46 /* CF_END_TAG_START */]: "</cf",
    [47 /* CF_TAG_END_VOID */]: "/>",
    [48 /* CF_TAG_COMMENT_START */]: "<!---",
    [49 /* CF_TAG_COMMENT_END */]: "--->",
    [50 /* LIT_AND */]: "and",
    [51 /* LIT_CONTAINS */]: "contains",
    [52 /* LIT_DOES_NOT_CONTAIN */]: "does not contain",
    [53 /* LIT_EQ */]: "eq",
    [54 /* LIT_GT */]: "gt",
    [55 /* LIT_GTE */]: "gte",
    [56 /* LIT_GE */]: "ge",
    [57 /* LIT_INCLUDES */]: "includes",
    [58 /* LIT_IS */]: "is",
    [59 /* LIT_IS_NOT */]: "is not",
    [60 /* LIT_LT */]: "lt",
    [61 /* LIT_LE */]: "le",
    [62 /* LIT_LTE */]: "lte",
    [63 /* LIT_MOD */]: "mod",
    [64 /* LIT_NEQ */]: "neq",
    [65 /* LIT_OR */]: "or",
    [66 /* LIT_XOR */]: "xor",
    [67 /* KW_BREAK */]: "break",
    [68 /* KW_CASE */]: "case",
    [69 /* KW_CATCH */]: "catch",
    [70 /* KW_DEFAULT */]: "default",
    [71 /* KW_DO */]: "do",
    [72 /* KW_ELSE */]: "else",
    [73 /* KW_FINAL */]: "final",
    [74 /* KW_FOR */]: "for",
    [75 /* KW_FUNCTION */]: "function",
    [76 /* KW_IF */]: "if",
    [77 /* KW_IMPORT */]: "import",
    [78 /* KW_NEW */]: "new",
    [79 /* KW_RETURN */]: "return",
    [80 /* KW_TRY */]: "try",
    [81 /* KW_VAR */]: "var",
    [82 /* KW_WHILE */]: "while",
};
class Token {
    constructor(type, fromOrRange, toExclusive) {
        this.type = type;
        if (typeof fromOrRange === "number") {
            this.range = new scanner_1.SourceRange(fromOrRange, toExclusive);
        }
        else {
            this.range = fromOrRange;
        }
    }
    static Nil() {
        return new Token(0 /* NIL */, scanner_1.SourceRange.Nil());
    }
    isNil() {
        return this.range.fromInclusive === -1 && this.range.toExclusive === -1;
    }
}
exports.Token = Token;
class Tokenizer {
    constructor(scanner) {
        this.token_ = Token.Nil();
        this.scanner_ = scanner;
    }
    prime(mode) {
        // ?
    }
    setToken(type, from, to) {
        this.token_ = new Token(type, from, to);
        return this.token_;
    }
    getIndex() {
        return this.scanner_.getIndex();
    }
    restoreIndex(index) {
        this.scanner_.restoreIndex(index);
    }
    getArtificalEndLimit() {
        return this.scanner_.getArtificalEndLimit();
    }
    setArtificialEndLimit(offset) {
        this.scanner_.setArtificialEndLimit(offset);
    }
    clearArtificalEndLimit() {
        this.scanner_.clearArtificalEndLimit();
    }
    consumeCurrentCharAs(type) {
        const from = this.getIndex();
        this.scanner_.next();
        return this.setToken(type, from, this.getIndex());
    }
    getTokenText(token) {
        const saveIndex = this.getIndex();
        const result = [];
        this.restoreIndex(token.range.fromInclusive);
        while (this.getIndex() != token.range.toExclusive) {
            result.push(String.fromCharCode(this.scanner_.next().codepoint));
        }
        this.restoreIndex(saveIndex);
        return result.join("");
    }
    lexeme() {
        const from = this.getIndex();
        if (!this.scanner_.maybeEat(/[$_a-z][$_a-z0-9]*/iy))
            throw "expected a lexeme";
        return this.setToken(4 /* LEXEME */, from, this.getIndex());
    }
    next(mode) {
        if (!this.scanner_.hasNext()) {
            return this.token_;
        }
        const tag = mode == 0 /* tag */;
        const script = mode == 1 /* script */;
        const from = this.getIndex();
        const c = this.scanner_.peek().codepoint;
        switch (c) {
            case 0:
                return this.setToken(1 /* EOF */, from, from);
            case 60 /* LEFT_ANGLE */:
                if (tag && this.scanner_.maybeEat(/<\s*cf/iy))
                    return this.setToken(45 /* CF_START_TAG_START */, from, this.getIndex());
                else if (tag && this.scanner_.maybeEat(/<\s*\/\s*cf/iy))
                    return this.setToken(46 /* CF_END_TAG_START */, from, this.getIndex());
                else if (tag && this.scanner_.maybeEat(/<!---/iy))
                    return this.setToken(48 /* CF_TAG_COMMENT_START */, from, this.getIndex());
                else if (this.scanner_.maybeEat(/<=/iy))
                    return this.setToken(26 /* LEFT_ANGLE_EQUAL */, from, this.getIndex());
                else
                    return this.consumeCurrentCharAs(25 /* LEFT_ANGLE */);
            case 62 /* RIGHT_ANGLE */:
                if (script && this.scanner_.maybeEat(/>=/iy))
                    return this.setToken(39 /* RIGHT_ANGLE_EQUAL */, from, this.getIndex());
                return this.consumeCurrentCharAs(38 /* RIGHT_ANGLE */);
            case 61 /* EQUAL */:
                if (this.scanner_.maybeEat(/==/iy))
                    return this.setToken(11 /* DBL_EQUAL */, from, this.getIndex());
                else if (this.scanner_.maybeEat(/=>/iy))
                    return this.setToken(18 /* EQUAL_RIGHT_ANGLE */, from, this.getIndex());
                else
                    return this.consumeCurrentCharAs(17 /* EQUAL */);
            case 35 /* HASH */: return this.consumeCurrentCharAs(24 /* HASH */);
            case 37 /* PERCENT */: return this.consumeCurrentCharAs(32 /* PERCENT */);
            case 40 /* LEFT_PAREN */: return this.consumeCurrentCharAs(29 /* LEFT_PAREN */);
            case 41 /* RIGHT_PAREN */: return this.consumeCurrentCharAs(42 /* RIGHT_PAREN */);
            case 91 /* LEFT_BRACKET */: return this.consumeCurrentCharAs(28 /* LEFT_BRACKET */);
            case 93 /* RIGHT_BRACKET */: return this.consumeCurrentCharAs(41 /* RIGHT_BRACKET */);
            case 123 /* LEFT_BRACE */: return this.consumeCurrentCharAs(27 /* LEFT_BRACE */);
            case 125 /* RIGHT_BRACE */: return this.consumeCurrentCharAs(40 /* RIGHT_BRACE */);
            case 94 /* CARET */: return this.consumeCurrentCharAs(8 /* CARET */);
            case 39 /* QUOTE_SINGLE */: return this.consumeCurrentCharAs(36 /* QUOTE_SINGLE */);
            case 34 /* QUOTE_DOUBLE */: return this.consumeCurrentCharAs(37 /* QUOTE_DOUBLE */);
            case 44 /* COMMA */: return this.consumeCurrentCharAs(9 /* COMMA */);
            case 32 /* SPACE */: //    [[fallthrough]];
            case 9 /* TAB */: // \t [[fallthrough]];
            case 13 /* CR */: // \r [[fallthrough]];
            case 10 /* LF */: // \n [[fallthrough]];
                // more of a "definite" eat here
                this.scanner_.maybeEat(/\s+/iy);
                return this.setToken(3 /* WHITESPACE */, from, this.getIndex());
            case 33 /* EXCLAMATION */:
                if (this.scanner_.maybeEat(/!=/iy))
                    return this.setToken(20 /* EXCLAMATION_EQUAL */, from, this.getIndex());
                else
                    return this.consumeCurrentCharAs(19 /* EXCLAMATION */);
            case 124 /* PIPE */:
                if (this.scanner_.maybeEat(/\|\|/iy))
                    return this.setToken(14 /* DBL_PIPE */, from, this.getIndex());
                else
                    return this.consumeCurrentCharAs(33 /* PIPE */);
            case 46 /* DOT */:
                if (this.scanner_.maybeEat(/\.\d+/iy))
                    return this.setToken(5 /* NUMBER */, from, this.getIndex());
                return this.consumeCurrentCharAs(16 /* DOT */);
            case 43 /* PLUS */:
                if (this.scanner_.maybeEat(/\+\+/iy))
                    return this.setToken(15 /* DBL_PLUS */, from, this.getIndex());
                if (this.scanner_.maybeEat(/\+=/iy))
                    return this.setToken(35 /* PLUS_EQUAL */, from, this.getIndex());
                else
                    return this.consumeCurrentCharAs(34 /* PLUS */);
            case 45 /* MINUS */:
                if (this.scanner_.maybeEat(/--->/iy))
                    return this.setToken(49 /* CF_TAG_COMMENT_END */, from, this.getIndex());
                if (this.scanner_.maybeEat(/--/iy))
                    return this.setToken(13 /* DBL_MINUS */, from, this.getIndex());
                if (this.scanner_.maybeEat(/-=/iy))
                    return this.setToken(31 /* MINUS_EQUAL */, from, this.getIndex());
                else
                    return this.consumeCurrentCharAs(30 /* MINUS */);
            case 38 /* AMPERSAND */:
                if (this.scanner_.maybeEat(/&&/iy))
                    return this.setToken(10 /* DBL_AMPERSAND */, from, this.getIndex());
                if (this.scanner_.maybeEat(/&=/iy))
                    return this.setToken(7 /* AMPERSAND_EQUAL */, from, this.getIndex());
                else
                    return this.consumeCurrentCharAs(6 /* AMPERSAND */);
            case 42 /* STAR */:
                if (this.scanner_.maybeEat(/\*=/iy))
                    return this.setToken(44 /* STAR_EQUAL */, from, this.getIndex());
                else
                    return this.consumeCurrentCharAs(43 /* STAR */);
            case 47 /* FORWARD_SLASH */:
                if (script && this.scanner_.maybeEat(/\/\//iy))
                    return this.setToken(12 /* DBL_FORWARD_SLASH */, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/\/*/iy))
                    return this.setToken(23 /* FORWARD_SLASH_STAR */, from, this.getIndex());
                else if (tag && this.scanner_.maybeEat(/\/\s*>/iy))
                    return this.setToken(47 /* CF_TAG_END_VOID */, from, this.getIndex());
                else if (this.scanner_.maybeEat(/\/=/iy))
                    return this.setToken(22 /* FORWARD_SLASH_EQUAL */, from, this.getIndex());
                else
                    return this.consumeCurrentCharAs(21 /* FORWARD_SLASH */);
            case 97 /* a */: // [[fallthrough]];
            case 65 /* A */:
                if (this.scanner_.maybeEat(/and/iy))
                    return this.setToken(50 /* LIT_AND */, from, this.getIndex());
                else
                    return this.lexeme();
            case 98 /* b */: // [[fallthrough]];
            case 66 /* B */:
                if (script && this.scanner_.maybeEat(/break/iy))
                    return this.setToken(67 /* KW_BREAK */, from, this.getIndex());
                else
                    return this.lexeme();
            case 99 /* c */: // [[fallthrough]];
            case 67 /* C */:
                if (script && this.scanner_.maybeEat(/catch/iy))
                    return this.setToken(69 /* KW_CATCH */, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/case/iy))
                    return this.setToken(68 /* KW_CASE */, from, this.getIndex());
                else if (this.scanner_.maybeEat(/contains/iy))
                    return this.setToken(51 /* LIT_CONTAINS */, from, this.getIndex());
                else
                    return this.lexeme();
            case 100 /* d */: // [[fallthrough]];
            case 68 /* D */:
                if (this.scanner_.maybeEat(/does\s+not\s+contain/iy))
                    return this.setToken(52 /* LIT_DOES_NOT_CONTAIN */, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/default/iy))
                    return this.setToken(70 /* KW_DEFAULT */, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/do/iy))
                    return this.setToken(71 /* KW_DO */, from, this.getIndex());
                else
                    return this.lexeme();
            case 101 /* e */: // [[fallthrough]];
            case 69 /* E */:
                if (this.scanner_.maybeEat(/eq/iy))
                    return this.setToken(53 /* LIT_EQ */, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/else/iy))
                    return this.setToken(72 /* KW_ELSE */, from, this.getIndex());
                else
                    return this.lexeme();
            case 102 /* f */: // [[fallthrough]];
            case 70 /* F */:
                if (this.scanner_.maybeEat(/final/iy))
                    return this.setToken(73 /* KW_FINAL */, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/for/iy))
                    return this.setToken(74 /* KW_FOR */, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/function/iy))
                    return this.setToken(75 /* KW_FUNCTION */, from, this.getIndex());
                else
                    return this.lexeme();
            case 103 /* g */: // [[fallthrough]];
            case 71 /* G */:
                if (this.scanner_.maybeEat(/gte/iy))
                    return this.setToken(55 /* LIT_GTE */, from, this.getIndex());
                else if (this.scanner_.maybeEat(/gt/iy))
                    return this.setToken(54 /* LIT_GT */, from, this.getIndex());
                else if (this.scanner_.maybeEat(/ge/iy))
                    return this.setToken(56 /* LIT_GE */, from, this.getIndex());
                else
                    return this.lexeme();
            case 105 /* i */: // [[fallthrough]];
            case 73 /* I */:
                if (script && this.scanner_.maybeEat(/if/iy))
                    return this.setToken(76 /* KW_IF */, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/import/iy))
                    return this.setToken(77 /* KW_IMPORT */, from, this.getIndex());
                else if (this.scanner_.maybeEat(/is\s+not/iy))
                    return this.setToken(59 /* LIT_IS_NOT */, from, this.getIndex());
                else if (this.scanner_.maybeEat(/is/iy))
                    return this.setToken(58 /* LIT_IS */, from, this.getIndex());
                else
                    return this.lexeme();
            case 108 /* l */: // [[fallthrough]];
            case 76 /* L */:
                if (this.scanner_.maybeEat(/lte/iy))
                    return this.setToken(62 /* LIT_LTE */, from, this.getIndex());
                else if (this.scanner_.maybeEat(/le/iy))
                    return this.setToken(61 /* LIT_LE */, from, this.getIndex());
                else if (this.scanner_.maybeEat(/lt/iy))
                    return this.setToken(60 /* LIT_LT */, from, this.getIndex());
                else
                    return this.lexeme();
            case 109 /* m */: // [[fallthrough]];
            case 77 /* M */:
                if (this.scanner_.maybeEat(/mod/iy))
                    return this.setToken(63 /* LIT_MOD */, from, this.getIndex());
                else
                    return this.lexeme();
            case 110 /* n */: // [[fallthrough]];
            case 78 /* N */:
                if (this.scanner_.maybeEat(/neq/iy))
                    return this.setToken(64 /* LIT_NEQ */, from, this.getIndex());
                else if (this.scanner_.maybeEat(/new/iy))
                    return this.setToken(78 /* KW_NEW */, from, this.getIndex());
                else
                    return this.lexeme();
            case 111 /* o */: // [[fallthrough]];
            case 79 /* O */:
                if (this.scanner_.maybeEat(/or/iy))
                    return this.setToken(65 /* LIT_OR */, from, this.getIndex());
                else
                    return this.lexeme();
            case 114 /* r */: // [[fallthrough]];
            case 82 /* R */:
                if (script && this.scanner_.maybeEat(/return/iy))
                    return this.setToken(79 /* KW_RETURN */, from, this.getIndex());
                else
                    return this.lexeme();
            case 116 /* t */: // [[fallthrough]];
            case 84 /* T */:
                if (script && this.scanner_.maybeEat(/try/iy))
                    return this.setToken(80 /* KW_TRY */, from, this.getIndex());
                else
                    return this.lexeme();
            case 118 /* v */: // [[fallthrough]];
            case 86 /* V */:
                if (this.scanner_.maybeEat(/var/iy))
                    return this.setToken(81 /* KW_VAR */, from, this.getIndex());
                else
                    return this.lexeme();
            case 119 /* w */: // [[fallthrough]];
            case 87 /* W */:
                if (script && this.scanner_.maybeEat(/while/iy))
                    return this.setToken(82 /* KW_WHILE */, from, this.getIndex());
                else
                    return this.lexeme();
            case 120 /* x */: // [[fallthrough]];
            case 88 /* X */:
                if (this.scanner_.maybeEat(/xor/iy))
                    return this.setToken(66 /* LIT_XOR */, from, this.getIndex());
                else
                    return this.lexeme();
            /*case '\xEF':
                if (this.scanner_.maybeEat(/\xEF\xBB\xBF/, TokenType.BOM_UTF_8)) return token_;*/
            default:
                if (this.scanner_.maybeEat(/\d+e[+-]?\d+(\.\d+)?|\d+(\.\d+)?/iy))
                    return this.setToken(5 /* NUMBER */, from, this.getIndex());
                return this.lexeme();
        }
    }
    peek(jump, mode) {
        const saveIndex = this.getIndex();
        let result;
        do {
            result = this.next(mode);
        } while (jump--);
        this.restoreIndex(saveIndex);
        return result;
    }
}
exports.Tokenizer = Tokenizer;
//# sourceMappingURL=tokenizer.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/balanced-match/index.js":
/*!*********************************************************************!*\
  !*** ./src/lang-server/client/node_modules/balanced-match/index.js ***!
  \*********************************************************************/
/***/ ((module) => {

"use strict";

module.exports = balanced;
function balanced(a, b, str) {
  if (a instanceof RegExp) a = maybeMatch(a, str);
  if (b instanceof RegExp) b = maybeMatch(b, str);

  var r = range(a, b, str);

  return r && {
    start: r[0],
    end: r[1],
    pre: str.slice(0, r[0]),
    body: str.slice(r[0] + a.length, r[1]),
    post: str.slice(r[1] + b.length)
  };
}

function maybeMatch(reg, str) {
  var m = str.match(reg);
  return m ? m[0] : null;
}

balanced.range = range;
function range(a, b, str) {
  var begs, beg, left, right, result;
  var ai = str.indexOf(a);
  var bi = str.indexOf(b, ai + 1);
  var i = ai;

  if (ai >= 0 && bi > 0) {
    begs = [];
    left = str.length;

    while (i >= 0 && !result) {
      if (i == ai) {
        begs.push(i);
        ai = str.indexOf(a, i + 1);
      } else if (begs.length == 1) {
        result = [ begs.pop(), bi ];
      } else {
        beg = begs.pop();
        if (beg < left) {
          left = beg;
          right = bi;
        }

        bi = str.indexOf(b, i + 1);
      }

      i = ai < bi && ai >= 0 ? ai : bi;
    }

    if (begs.length) {
      result = [ left, right ];
    }
  }

  return result;
}


/***/ }),

/***/ "./src/lang-server/client/node_modules/brace-expansion/index.js":
/*!**********************************************************************!*\
  !*** ./src/lang-server/client/node_modules/brace-expansion/index.js ***!
  \**********************************************************************/
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

var concatMap = __webpack_require__(/*! concat-map */ "./src/lang-server/client/node_modules/concat-map/index.js");
var balanced = __webpack_require__(/*! balanced-match */ "./src/lang-server/client/node_modules/balanced-match/index.js");

module.exports = expandTop;

var escSlash = '\0SLASH'+Math.random()+'\0';
var escOpen = '\0OPEN'+Math.random()+'\0';
var escClose = '\0CLOSE'+Math.random()+'\0';
var escComma = '\0COMMA'+Math.random()+'\0';
var escPeriod = '\0PERIOD'+Math.random()+'\0';

function numeric(str) {
  return parseInt(str, 10) == str
    ? parseInt(str, 10)
    : str.charCodeAt(0);
}

function escapeBraces(str) {
  return str.split('\\\\').join(escSlash)
            .split('\\{').join(escOpen)
            .split('\\}').join(escClose)
            .split('\\,').join(escComma)
            .split('\\.').join(escPeriod);
}

function unescapeBraces(str) {
  return str.split(escSlash).join('\\')
            .split(escOpen).join('{')
            .split(escClose).join('}')
            .split(escComma).join(',')
            .split(escPeriod).join('.');
}


// Basically just str.split(","), but handling cases
// where we have nested braced sections, which should be
// treated as individual members, like {a,{b,c},d}
function parseCommaParts(str) {
  if (!str)
    return [''];

  var parts = [];
  var m = balanced('{', '}', str);

  if (!m)
    return str.split(',');

  var pre = m.pre;
  var body = m.body;
  var post = m.post;
  var p = pre.split(',');

  p[p.length-1] += '{' + body + '}';
  var postParts = parseCommaParts(post);
  if (post.length) {
    p[p.length-1] += postParts.shift();
    p.push.apply(p, postParts);
  }

  parts.push.apply(parts, p);

  return parts;
}

function expandTop(str) {
  if (!str)
    return [];

  // I don't know why Bash 4.3 does this, but it does.
  // Anything starting with {} will have the first two bytes preserved
  // but *only* at the top level, so {},a}b will not expand to anything,
  // but a{},b}c will be expanded to [a}c,abc].
  // One could argue that this is a bug in Bash, but since the goal of
  // this module is to match Bash's rules, we escape a leading {}
  if (str.substr(0, 2) === '{}') {
    str = '\\{\\}' + str.substr(2);
  }

  return expand(escapeBraces(str), true).map(unescapeBraces);
}

function identity(e) {
  return e;
}

function embrace(str) {
  return '{' + str + '}';
}
function isPadded(el) {
  return /^-?0\d/.test(el);
}

function lte(i, y) {
  return i <= y;
}
function gte(i, y) {
  return i >= y;
}

function expand(str, isTop) {
  var expansions = [];

  var m = balanced('{', '}', str);
  if (!m || /\$$/.test(m.pre)) return [str];

  var isNumericSequence = /^-?\d+\.\.-?\d+(?:\.\.-?\d+)?$/.test(m.body);
  var isAlphaSequence = /^[a-zA-Z]\.\.[a-zA-Z](?:\.\.-?\d+)?$/.test(m.body);
  var isSequence = isNumericSequence || isAlphaSequence;
  var isOptions = m.body.indexOf(',') >= 0;
  if (!isSequence && !isOptions) {
    // {a},b}
    if (m.post.match(/,.*\}/)) {
      str = m.pre + '{' + m.body + escClose + m.post;
      return expand(str);
    }
    return [str];
  }

  var n;
  if (isSequence) {
    n = m.body.split(/\.\./);
  } else {
    n = parseCommaParts(m.body);
    if (n.length === 1) {
      // x{{a,b}}y ==> x{a}y x{b}y
      n = expand(n[0], false).map(embrace);
      if (n.length === 1) {
        var post = m.post.length
          ? expand(m.post, false)
          : [''];
        return post.map(function(p) {
          return m.pre + n[0] + p;
        });
      }
    }
  }

  // at this point, n is the parts, and we know it's not a comma set
  // with a single entry.

  // no need to expand pre, since it is guaranteed to be free of brace-sets
  var pre = m.pre;
  var post = m.post.length
    ? expand(m.post, false)
    : [''];

  var N;

  if (isSequence) {
    var x = numeric(n[0]);
    var y = numeric(n[1]);
    var width = Math.max(n[0].length, n[1].length)
    var incr = n.length == 3
      ? Math.abs(numeric(n[2]))
      : 1;
    var test = lte;
    var reverse = y < x;
    if (reverse) {
      incr *= -1;
      test = gte;
    }
    var pad = n.some(isPadded);

    N = [];

    for (var i = x; test(i, y); i += incr) {
      var c;
      if (isAlphaSequence) {
        c = String.fromCharCode(i);
        if (c === '\\')
          c = '';
      } else {
        c = String(i);
        if (pad) {
          var need = width - c.length;
          if (need > 0) {
            var z = new Array(need + 1).join('0');
            if (i < 0)
              c = '-' + z + c.slice(1);
            else
              c = z + c;
          }
        }
      }
      N.push(c);
    }
  } else {
    N = concatMap(n, function(el) { return expand(el, false) });
  }

  for (var j = 0; j < N.length; j++) {
    for (var k = 0; k < post.length; k++) {
      var expansion = pre + N[j] + post[k];
      if (!isTop || isSequence || expansion)
        expansions.push(expansion);
    }
  }

  return expansions;
}



/***/ }),

/***/ "./src/lang-server/client/node_modules/concat-map/index.js":
/*!*****************************************************************!*\
  !*** ./src/lang-server/client/node_modules/concat-map/index.js ***!
  \*****************************************************************/
/***/ ((module) => {

module.exports = function (xs, fn) {
    var res = [];
    for (var i = 0; i < xs.length; i++) {
        var x = fn(xs[i], i);
        if (isArray(x)) res.push.apply(res, x);
        else res.push(x);
    }
    return res;
};

var isArray = Array.isArray || function (xs) {
    return Object.prototype.toString.call(xs) === '[object Array]';
};


/***/ }),

/***/ "./src/lang-server/client/node_modules/minimatch/minimatch.js":
/*!********************************************************************!*\
  !*** ./src/lang-server/client/node_modules/minimatch/minimatch.js ***!
  \********************************************************************/
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

module.exports = minimatch
minimatch.Minimatch = Minimatch

var path = { sep: '/' }
try {
  path = __webpack_require__(/*! path */ "path")
} catch (er) {}

var GLOBSTAR = minimatch.GLOBSTAR = Minimatch.GLOBSTAR = {}
var expand = __webpack_require__(/*! brace-expansion */ "./src/lang-server/client/node_modules/brace-expansion/index.js")

var plTypes = {
  '!': { open: '(?:(?!(?:', close: '))[^/]*?)'},
  '?': { open: '(?:', close: ')?' },
  '+': { open: '(?:', close: ')+' },
  '*': { open: '(?:', close: ')*' },
  '@': { open: '(?:', close: ')' }
}

// any single thing other than /
// don't need to escape / when using new RegExp()
var qmark = '[^/]'

// * => any number of characters
var star = qmark + '*?'

// ** when dots are allowed.  Anything goes, except .. and .
// not (^ or / followed by one or two dots followed by $ or /),
// followed by anything, any number of times.
var twoStarDot = '(?:(?!(?:\\\/|^)(?:\\.{1,2})($|\\\/)).)*?'

// not a ^ or / followed by a dot,
// followed by anything, any number of times.
var twoStarNoDot = '(?:(?!(?:\\\/|^)\\.).)*?'

// characters that need to be escaped in RegExp.
var reSpecials = charSet('().*{}+?[]^$\\!')

// "abc" -> { a:true, b:true, c:true }
function charSet (s) {
  return s.split('').reduce(function (set, c) {
    set[c] = true
    return set
  }, {})
}

// normalizes slashes.
var slashSplit = /\/+/

minimatch.filter = filter
function filter (pattern, options) {
  options = options || {}
  return function (p, i, list) {
    return minimatch(p, pattern, options)
  }
}

function ext (a, b) {
  a = a || {}
  b = b || {}
  var t = {}
  Object.keys(b).forEach(function (k) {
    t[k] = b[k]
  })
  Object.keys(a).forEach(function (k) {
    t[k] = a[k]
  })
  return t
}

minimatch.defaults = function (def) {
  if (!def || !Object.keys(def).length) return minimatch

  var orig = minimatch

  var m = function minimatch (p, pattern, options) {
    return orig.minimatch(p, pattern, ext(def, options))
  }

  m.Minimatch = function Minimatch (pattern, options) {
    return new orig.Minimatch(pattern, ext(def, options))
  }

  return m
}

Minimatch.defaults = function (def) {
  if (!def || !Object.keys(def).length) return Minimatch
  return minimatch.defaults(def).Minimatch
}

function minimatch (p, pattern, options) {
  if (typeof pattern !== 'string') {
    throw new TypeError('glob pattern string required')
  }

  if (!options) options = {}

  // shortcut: comments match nothing.
  if (!options.nocomment && pattern.charAt(0) === '#') {
    return false
  }

  // "" only matches ""
  if (pattern.trim() === '') return p === ''

  return new Minimatch(pattern, options).match(p)
}

function Minimatch (pattern, options) {
  if (!(this instanceof Minimatch)) {
    return new Minimatch(pattern, options)
  }

  if (typeof pattern !== 'string') {
    throw new TypeError('glob pattern string required')
  }

  if (!options) options = {}
  pattern = pattern.trim()

  // windows support: need to use /, not \
  if (path.sep !== '/') {
    pattern = pattern.split(path.sep).join('/')
  }

  this.options = options
  this.set = []
  this.pattern = pattern
  this.regexp = null
  this.negate = false
  this.comment = false
  this.empty = false

  // make the set of regexps etc.
  this.make()
}

Minimatch.prototype.debug = function () {}

Minimatch.prototype.make = make
function make () {
  // don't do it more than once.
  if (this._made) return

  var pattern = this.pattern
  var options = this.options

  // empty patterns and comments match nothing.
  if (!options.nocomment && pattern.charAt(0) === '#') {
    this.comment = true
    return
  }
  if (!pattern) {
    this.empty = true
    return
  }

  // step 1: figure out negation, etc.
  this.parseNegate()

  // step 2: expand braces
  var set = this.globSet = this.braceExpand()

  if (options.debug) this.debug = console.error

  this.debug(this.pattern, set)

  // step 3: now we have a set, so turn each one into a series of path-portion
  // matching patterns.
  // These will be regexps, except in the case of "**", which is
  // set to the GLOBSTAR object for globstar behavior,
  // and will not contain any / characters
  set = this.globParts = set.map(function (s) {
    return s.split(slashSplit)
  })

  this.debug(this.pattern, set)

  // glob --> regexps
  set = set.map(function (s, si, set) {
    return s.map(this.parse, this)
  }, this)

  this.debug(this.pattern, set)

  // filter out everything that didn't compile properly.
  set = set.filter(function (s) {
    return s.indexOf(false) === -1
  })

  this.debug(this.pattern, set)

  this.set = set
}

Minimatch.prototype.parseNegate = parseNegate
function parseNegate () {
  var pattern = this.pattern
  var negate = false
  var options = this.options
  var negateOffset = 0

  if (options.nonegate) return

  for (var i = 0, l = pattern.length
    ; i < l && pattern.charAt(i) === '!'
    ; i++) {
    negate = !negate
    negateOffset++
  }

  if (negateOffset) this.pattern = pattern.substr(negateOffset)
  this.negate = negate
}

// Brace expansion:
// a{b,c}d -> abd acd
// a{b,}c -> abc ac
// a{0..3}d -> a0d a1d a2d a3d
// a{b,c{d,e}f}g -> abg acdfg acefg
// a{b,c}d{e,f}g -> abdeg acdeg abdeg abdfg
//
// Invalid sets are not expanded.
// a{2..}b -> a{2..}b
// a{b}c -> a{b}c
minimatch.braceExpand = function (pattern, options) {
  return braceExpand(pattern, options)
}

Minimatch.prototype.braceExpand = braceExpand

function braceExpand (pattern, options) {
  if (!options) {
    if (this instanceof Minimatch) {
      options = this.options
    } else {
      options = {}
    }
  }

  pattern = typeof pattern === 'undefined'
    ? this.pattern : pattern

  if (typeof pattern === 'undefined') {
    throw new TypeError('undefined pattern')
  }

  if (options.nobrace ||
    !pattern.match(/\{.*\}/)) {
    // shortcut. no need to expand.
    return [pattern]
  }

  return expand(pattern)
}

// parse a component of the expanded set.
// At this point, no pattern may contain "/" in it
// so we're going to return a 2d array, where each entry is the full
// pattern, split on '/', and then turned into a regular expression.
// A regexp is made at the end which joins each array with an
// escaped /, and another full one which joins each regexp with |.
//
// Following the lead of Bash 4.1, note that "**" only has special meaning
// when it is the *only* thing in a path portion.  Otherwise, any series
// of * is equivalent to a single *.  Globstar behavior is enabled by
// default, and can be disabled by setting options.noglobstar.
Minimatch.prototype.parse = parse
var SUBPARSE = {}
function parse (pattern, isSub) {
  if (pattern.length > 1024 * 64) {
    throw new TypeError('pattern is too long')
  }

  var options = this.options

  // shortcuts
  if (!options.noglobstar && pattern === '**') return GLOBSTAR
  if (pattern === '') return ''

  var re = ''
  var hasMagic = !!options.nocase
  var escaping = false
  // ? => one single character
  var patternListStack = []
  var negativeLists = []
  var stateChar
  var inClass = false
  var reClassStart = -1
  var classStart = -1
  // . and .. never match anything that doesn't start with .,
  // even when options.dot is set.
  var patternStart = pattern.charAt(0) === '.' ? '' // anything
  // not (start or / followed by . or .. followed by / or end)
  : options.dot ? '(?!(?:^|\\\/)\\.{1,2}(?:$|\\\/))'
  : '(?!\\.)'
  var self = this

  function clearStateChar () {
    if (stateChar) {
      // we had some state-tracking character
      // that wasn't consumed by this pass.
      switch (stateChar) {
        case '*':
          re += star
          hasMagic = true
        break
        case '?':
          re += qmark
          hasMagic = true
        break
        default:
          re += '\\' + stateChar
        break
      }
      self.debug('clearStateChar %j %j', stateChar, re)
      stateChar = false
    }
  }

  for (var i = 0, len = pattern.length, c
    ; (i < len) && (c = pattern.charAt(i))
    ; i++) {
    this.debug('%s\t%s %s %j', pattern, i, re, c)

    // skip over any that are escaped.
    if (escaping && reSpecials[c]) {
      re += '\\' + c
      escaping = false
      continue
    }

    switch (c) {
      case '/':
        // completely not allowed, even escaped.
        // Should already be path-split by now.
        return false

      case '\\':
        clearStateChar()
        escaping = true
      continue

      // the various stateChar values
      // for the "extglob" stuff.
      case '?':
      case '*':
      case '+':
      case '@':
      case '!':
        this.debug('%s\t%s %s %j <-- stateChar', pattern, i, re, c)

        // all of those are literals inside a class, except that
        // the glob [!a] means [^a] in regexp
        if (inClass) {
          this.debug('  in class')
          if (c === '!' && i === classStart + 1) c = '^'
          re += c
          continue
        }

        // if we already have a stateChar, then it means
        // that there was something like ** or +? in there.
        // Handle the stateChar, then proceed with this one.
        self.debug('call clearStateChar %j', stateChar)
        clearStateChar()
        stateChar = c
        // if extglob is disabled, then +(asdf|foo) isn't a thing.
        // just clear the statechar *now*, rather than even diving into
        // the patternList stuff.
        if (options.noext) clearStateChar()
      continue

      case '(':
        if (inClass) {
          re += '('
          continue
        }

        if (!stateChar) {
          re += '\\('
          continue
        }

        patternListStack.push({
          type: stateChar,
          start: i - 1,
          reStart: re.length,
          open: plTypes[stateChar].open,
          close: plTypes[stateChar].close
        })
        // negation is (?:(?!js)[^/]*)
        re += stateChar === '!' ? '(?:(?!(?:' : '(?:'
        this.debug('plType %j %j', stateChar, re)
        stateChar = false
      continue

      case ')':
        if (inClass || !patternListStack.length) {
          re += '\\)'
          continue
        }

        clearStateChar()
        hasMagic = true
        var pl = patternListStack.pop()
        // negation is (?:(?!js)[^/]*)
        // The others are (?:<pattern>)<type>
        re += pl.close
        if (pl.type === '!') {
          negativeLists.push(pl)
        }
        pl.reEnd = re.length
      continue

      case '|':
        if (inClass || !patternListStack.length || escaping) {
          re += '\\|'
          escaping = false
          continue
        }

        clearStateChar()
        re += '|'
      continue

      // these are mostly the same in regexp and glob
      case '[':
        // swallow any state-tracking char before the [
        clearStateChar()

        if (inClass) {
          re += '\\' + c
          continue
        }

        inClass = true
        classStart = i
        reClassStart = re.length
        re += c
      continue

      case ']':
        //  a right bracket shall lose its special
        //  meaning and represent itself in
        //  a bracket expression if it occurs
        //  first in the list.  -- POSIX.2 2.8.3.2
        if (i === classStart + 1 || !inClass) {
          re += '\\' + c
          escaping = false
          continue
        }

        // handle the case where we left a class open.
        // "[z-a]" is valid, equivalent to "\[z-a\]"
        if (inClass) {
          // split where the last [ was, make sure we don't have
          // an invalid re. if so, re-walk the contents of the
          // would-be class to re-translate any characters that
          // were passed through as-is
          // TODO: It would probably be faster to determine this
          // without a try/catch and a new RegExp, but it's tricky
          // to do safely.  For now, this is safe and works.
          var cs = pattern.substring(classStart + 1, i)
          try {
            RegExp('[' + cs + ']')
          } catch (er) {
            // not a valid class!
            var sp = this.parse(cs, SUBPARSE)
            re = re.substr(0, reClassStart) + '\\[' + sp[0] + '\\]'
            hasMagic = hasMagic || sp[1]
            inClass = false
            continue
          }
        }

        // finish up the class.
        hasMagic = true
        inClass = false
        re += c
      continue

      default:
        // swallow any state char that wasn't consumed
        clearStateChar()

        if (escaping) {
          // no need
          escaping = false
        } else if (reSpecials[c]
          && !(c === '^' && inClass)) {
          re += '\\'
        }

        re += c

    } // switch
  } // for

  // handle the case where we left a class open.
  // "[abc" is valid, equivalent to "\[abc"
  if (inClass) {
    // split where the last [ was, and escape it
    // this is a huge pita.  We now have to re-walk
    // the contents of the would-be class to re-translate
    // any characters that were passed through as-is
    cs = pattern.substr(classStart + 1)
    sp = this.parse(cs, SUBPARSE)
    re = re.substr(0, reClassStart) + '\\[' + sp[0]
    hasMagic = hasMagic || sp[1]
  }

  // handle the case where we had a +( thing at the *end*
  // of the pattern.
  // each pattern list stack adds 3 chars, and we need to go through
  // and escape any | chars that were passed through as-is for the regexp.
  // Go through and escape them, taking care not to double-escape any
  // | chars that were already escaped.
  for (pl = patternListStack.pop(); pl; pl = patternListStack.pop()) {
    var tail = re.slice(pl.reStart + pl.open.length)
    this.debug('setting tail', re, pl)
    // maybe some even number of \, then maybe 1 \, followed by a |
    tail = tail.replace(/((?:\\{2}){0,64})(\\?)\|/g, function (_, $1, $2) {
      if (!$2) {
        // the | isn't already escaped, so escape it.
        $2 = '\\'
      }

      // need to escape all those slashes *again*, without escaping the
      // one that we need for escaping the | character.  As it works out,
      // escaping an even number of slashes can be done by simply repeating
      // it exactly after itself.  That's why this trick works.
      //
      // I am sorry that you have to see this.
      return $1 + $1 + $2 + '|'
    })

    this.debug('tail=%j\n   %s', tail, tail, pl, re)
    var t = pl.type === '*' ? star
      : pl.type === '?' ? qmark
      : '\\' + pl.type

    hasMagic = true
    re = re.slice(0, pl.reStart) + t + '\\(' + tail
  }

  // handle trailing things that only matter at the very end.
  clearStateChar()
  if (escaping) {
    // trailing \\
    re += '\\\\'
  }

  // only need to apply the nodot start if the re starts with
  // something that could conceivably capture a dot
  var addPatternStart = false
  switch (re.charAt(0)) {
    case '.':
    case '[':
    case '(': addPatternStart = true
  }

  // Hack to work around lack of negative lookbehind in JS
  // A pattern like: *.!(x).!(y|z) needs to ensure that a name
  // like 'a.xyz.yz' doesn't match.  So, the first negative
  // lookahead, has to look ALL the way ahead, to the end of
  // the pattern.
  for (var n = negativeLists.length - 1; n > -1; n--) {
    var nl = negativeLists[n]

    var nlBefore = re.slice(0, nl.reStart)
    var nlFirst = re.slice(nl.reStart, nl.reEnd - 8)
    var nlLast = re.slice(nl.reEnd - 8, nl.reEnd)
    var nlAfter = re.slice(nl.reEnd)

    nlLast += nlAfter

    // Handle nested stuff like *(*.js|!(*.json)), where open parens
    // mean that we should *not* include the ) in the bit that is considered
    // "after" the negated section.
    var openParensBefore = nlBefore.split('(').length - 1
    var cleanAfter = nlAfter
    for (i = 0; i < openParensBefore; i++) {
      cleanAfter = cleanAfter.replace(/\)[+*?]?/, '')
    }
    nlAfter = cleanAfter

    var dollar = ''
    if (nlAfter === '' && isSub !== SUBPARSE) {
      dollar = '$'
    }
    var newRe = nlBefore + nlFirst + nlAfter + dollar + nlLast
    re = newRe
  }

  // if the re is not "" at this point, then we need to make sure
  // it doesn't match against an empty path part.
  // Otherwise a/* will match a/, which it should not.
  if (re !== '' && hasMagic) {
    re = '(?=.)' + re
  }

  if (addPatternStart) {
    re = patternStart + re
  }

  // parsing just a piece of a larger pattern.
  if (isSub === SUBPARSE) {
    return [re, hasMagic]
  }

  // skip the regexp for non-magical patterns
  // unescape anything in it, though, so that it'll be
  // an exact match against a file etc.
  if (!hasMagic) {
    return globUnescape(pattern)
  }

  var flags = options.nocase ? 'i' : ''
  try {
    var regExp = new RegExp('^' + re + '$', flags)
  } catch (er) {
    // If it was an invalid regular expression, then it can't match
    // anything.  This trick looks for a character after the end of
    // the string, which is of course impossible, except in multi-line
    // mode, but it's not a /m regex.
    return new RegExp('$.')
  }

  regExp._glob = pattern
  regExp._src = re

  return regExp
}

minimatch.makeRe = function (pattern, options) {
  return new Minimatch(pattern, options || {}).makeRe()
}

Minimatch.prototype.makeRe = makeRe
function makeRe () {
  if (this.regexp || this.regexp === false) return this.regexp

  // at this point, this.set is a 2d array of partial
  // pattern strings, or "**".
  //
  // It's better to use .match().  This function shouldn't
  // be used, really, but it's pretty convenient sometimes,
  // when you just want to work with a regex.
  var set = this.set

  if (!set.length) {
    this.regexp = false
    return this.regexp
  }
  var options = this.options

  var twoStar = options.noglobstar ? star
    : options.dot ? twoStarDot
    : twoStarNoDot
  var flags = options.nocase ? 'i' : ''

  var re = set.map(function (pattern) {
    return pattern.map(function (p) {
      return (p === GLOBSTAR) ? twoStar
      : (typeof p === 'string') ? regExpEscape(p)
      : p._src
    }).join('\\\/')
  }).join('|')

  // must match entire pattern
  // ending in a * or ** will make it less strict.
  re = '^(?:' + re + ')$'

  // can match anything, as long as it's not this.
  if (this.negate) re = '^(?!' + re + ').*$'

  try {
    this.regexp = new RegExp(re, flags)
  } catch (ex) {
    this.regexp = false
  }
  return this.regexp
}

minimatch.match = function (list, pattern, options) {
  options = options || {}
  var mm = new Minimatch(pattern, options)
  list = list.filter(function (f) {
    return mm.match(f)
  })
  if (mm.options.nonull && !list.length) {
    list.push(pattern)
  }
  return list
}

Minimatch.prototype.match = match
function match (f, partial) {
  this.debug('match', f, this.pattern)
  // short-circuit in the case of busted things.
  // comments, etc.
  if (this.comment) return false
  if (this.empty) return f === ''

  if (f === '/' && partial) return true

  var options = this.options

  // windows: need to use /, not \
  if (path.sep !== '/') {
    f = f.split(path.sep).join('/')
  }

  // treat the test path as a set of pathparts.
  f = f.split(slashSplit)
  this.debug(this.pattern, 'split', f)

  // just ONE of the pattern sets in this.set needs to match
  // in order for it to be valid.  If negating, then just one
  // match means that we have failed.
  // Either way, return on the first hit.

  var set = this.set
  this.debug(this.pattern, 'set', set)

  // Find the basename of the path by looking for the last non-empty segment
  var filename
  var i
  for (i = f.length - 1; i >= 0; i--) {
    filename = f[i]
    if (filename) break
  }

  for (i = 0; i < set.length; i++) {
    var pattern = set[i]
    var file = f
    if (options.matchBase && pattern.length === 1) {
      file = [filename]
    }
    var hit = this.matchOne(file, pattern, partial)
    if (hit) {
      if (options.flipNegate) return true
      return !this.negate
    }
  }

  // didn't get any hits.  this is success if it's a negative
  // pattern, failure otherwise.
  if (options.flipNegate) return false
  return this.negate
}

// set partial to true to test if, for example,
// "/a/b" matches the start of "/*/b/*/d"
// Partial means, if you run out of file before you run
// out of pattern, then that's fine, as long as all
// the parts match.
Minimatch.prototype.matchOne = function (file, pattern, partial) {
  var options = this.options

  this.debug('matchOne',
    { 'this': this, file: file, pattern: pattern })

  this.debug('matchOne', file.length, pattern.length)

  for (var fi = 0,
      pi = 0,
      fl = file.length,
      pl = pattern.length
      ; (fi < fl) && (pi < pl)
      ; fi++, pi++) {
    this.debug('matchOne loop')
    var p = pattern[pi]
    var f = file[fi]

    this.debug(pattern, p, f)

    // should be impossible.
    // some invalid regexp stuff in the set.
    if (p === false) return false

    if (p === GLOBSTAR) {
      this.debug('GLOBSTAR', [pattern, p, f])

      // "**"
      // a/**/b/**/c would match the following:
      // a/b/x/y/z/c
      // a/x/y/z/b/c
      // a/b/x/b/x/c
      // a/b/c
      // To do this, take the rest of the pattern after
      // the **, and see if it would match the file remainder.
      // If so, return success.
      // If not, the ** "swallows" a segment, and try again.
      // This is recursively awful.
      //
      // a/**/b/**/c matching a/b/x/y/z/c
      // - a matches a
      // - doublestar
      //   - matchOne(b/x/y/z/c, b/**/c)
      //     - b matches b
      //     - doublestar
      //       - matchOne(x/y/z/c, c) -> no
      //       - matchOne(y/z/c, c) -> no
      //       - matchOne(z/c, c) -> no
      //       - matchOne(c, c) yes, hit
      var fr = fi
      var pr = pi + 1
      if (pr === pl) {
        this.debug('** at the end')
        // a ** at the end will just swallow the rest.
        // We have found a match.
        // however, it will not swallow /.x, unless
        // options.dot is set.
        // . and .. are *never* matched by **, for explosively
        // exponential reasons.
        for (; fi < fl; fi++) {
          if (file[fi] === '.' || file[fi] === '..' ||
            (!options.dot && file[fi].charAt(0) === '.')) return false
        }
        return true
      }

      // ok, let's see if we can swallow whatever we can.
      while (fr < fl) {
        var swallowee = file[fr]

        this.debug('\nglobstar while', file, fr, pattern, pr, swallowee)

        // XXX remove this slice.  Just pass the start index.
        if (this.matchOne(file.slice(fr), pattern.slice(pr), partial)) {
          this.debug('globstar found match!', fr, fl, swallowee)
          // found a match.
          return true
        } else {
          // can't swallow "." or ".." ever.
          // can only swallow ".foo" when explicitly asked.
          if (swallowee === '.' || swallowee === '..' ||
            (!options.dot && swallowee.charAt(0) === '.')) {
            this.debug('dot detected!', file, fr, pattern, pr)
            break
          }

          // ** swallows a segment, and continue.
          this.debug('globstar swallow a segment, and continue')
          fr++
        }
      }

      // no match was found.
      // However, in partial mode, we can't say this is necessarily over.
      // If there's more *pattern* left, then
      if (partial) {
        // ran out of file
        this.debug('\n>>> no match, partial?', file, fr, pattern, pr)
        if (fr === fl) return true
      }
      return false
    }

    // something other than **
    // non-magic patterns just have to match exactly
    // patterns with magic have been turned into regexps.
    var hit
    if (typeof p === 'string') {
      if (options.nocase) {
        hit = f.toLowerCase() === p.toLowerCase()
      } else {
        hit = f === p
      }
      this.debug('string match', p, f, hit)
    } else {
      hit = f.match(p)
      this.debug('pattern match', p, f, hit)
    }

    if (!hit) return false
  }

  // Note: ending in / means that we'll get a final ""
  // at the end of the pattern.  This can only match a
  // corresponding "" at the end of the file.
  // If the file ends in /, then it can only match a
  // a pattern that ends in /, unless the pattern just
  // doesn't have any more for it. But, a/b/ should *not*
  // match "a/b/*", even though "" matches against the
  // [^/]*? pattern, except in partial mode, where it might
  // simply not be reached yet.
  // However, a/b/ should still satisfy a/*

  // now either we fell off the end of the pattern, or we're done.
  if (fi === fl && pi === pl) {
    // ran out of pattern and filename at the same time.
    // an exact hit!
    return true
  } else if (fi === fl) {
    // ran out of file, but still had pattern left.
    // this is ok if we're doing the match as part of
    // a glob fs traversal.
    return partial
  } else if (pi === pl) {
    // ran out of pattern, still have file left.
    // this is only acceptable if we're on the very last
    // empty segment of a file with a trailing slash.
    // a/* should match a/b/
    var emptyFileEnd = (fi === fl - 1) && (file[fi] === '')
    return emptyFileEnd
  }

  // should be unreachable.
  throw new Error('wtf?')
}

// replace stuff like \* with *
function globUnescape (s) {
  return s.replace(/\\(.)/g, '$1')
}

function regExpEscape (s) {
  return s.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, '\\$&')
}


/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/browser.js":
/*!***********************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/browser.js ***!
  \***********************************************************************/
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ----------------------------------------------------------------------------------------- */


module.exports = __webpack_require__(/*! ./lib/browser/main */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/browser/main.js");

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/browser/main.js":
/*!********************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/browser/main.js ***!
  \********************************************************************************/
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createMessageConnection = exports.BrowserMessageWriter = exports.BrowserMessageReader = void 0;
const ril_1 = __webpack_require__(/*! ./ril */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/browser/ril.js");
// Install the browser runtime abstract.
ril_1.default.install();
const api_1 = __webpack_require__(/*! ../common/api */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/api.js");
__exportStar(__webpack_require__(/*! ../common/api */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/api.js"), exports);
class BrowserMessageReader extends api_1.AbstractMessageReader {
    constructor(context) {
        super();
        this._onData = new api_1.Emitter();
        this._messageListener = (event) => {
            this._onData.fire(event.data);
        };
        context.addEventListener('error', (event) => this.fireError(event));
        context.onmessage = this._messageListener;
    }
    listen(callback) {
        return this._onData.event(callback);
    }
}
exports.BrowserMessageReader = BrowserMessageReader;
class BrowserMessageWriter extends api_1.AbstractMessageWriter {
    constructor(context) {
        super();
        this.context = context;
        this.errorCount = 0;
        context.addEventListener('error', (event) => this.fireError(event));
    }
    write(msg) {
        try {
            this.context.postMessage(msg);
            return Promise.resolve();
        }
        catch (error) {
            this.handleError(error, msg);
            return Promise.reject(error);
        }
    }
    handleError(error, msg) {
        this.errorCount++;
        this.fireError(error, msg, this.errorCount);
    }
    end() {
    }
}
exports.BrowserMessageWriter = BrowserMessageWriter;
function createMessageConnection(reader, writer, logger, options) {
    if (logger === undefined) {
        logger = api_1.NullLogger;
    }
    if (api_1.ConnectionStrategy.is(options)) {
        options = { connectionStrategy: options };
    }
    return api_1.createMessageConnection(reader, writer, logger, options);
}
exports.createMessageConnection = createMessageConnection;
//# sourceMappingURL=main.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/browser/ril.js":
/*!*******************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/browser/ril.js ***!
  \*******************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const ral_1 = __webpack_require__(/*! ../common/ral */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/ral.js");
const disposable_1 = __webpack_require__(/*! ../common/disposable */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/disposable.js");
const events_1 = __webpack_require__(/*! ../common/events */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/events.js");
const messageBuffer_1 = __webpack_require__(/*! ../common/messageBuffer */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messageBuffer.js");
class MessageBuffer extends messageBuffer_1.AbstractMessageBuffer {
    constructor(encoding = 'utf-8') {
        super(encoding);
        this.asciiDecoder = new TextDecoder('ascii');
    }
    emptyBuffer() {
        return MessageBuffer.emptyBuffer;
    }
    fromString(value, _encoding) {
        return (new TextEncoder()).encode(value);
    }
    toString(value, encoding) {
        if (encoding === 'ascii') {
            return this.asciiDecoder.decode(value);
        }
        else {
            return (new TextDecoder(encoding)).decode(value);
        }
    }
    asNative(buffer, length) {
        if (length === undefined) {
            return buffer;
        }
        else {
            return buffer.slice(0, length);
        }
    }
    allocNative(length) {
        return new Uint8Array(length);
    }
}
MessageBuffer.emptyBuffer = new Uint8Array(0);
class ReadableStreamWrapper {
    constructor(socket) {
        this.socket = socket;
        this._onData = new events_1.Emitter();
        this._messageListener = (event) => {
            const blob = event.data;
            blob.arrayBuffer().then((buffer) => {
                this._onData.fire(new Uint8Array(buffer));
            });
        };
        this.socket.addEventListener('message', this._messageListener);
    }
    onClose(listener) {
        this.socket.addEventListener('close', listener);
        return disposable_1.Disposable.create(() => this.socket.removeEventListener('close', listener));
    }
    onError(listener) {
        this.socket.addEventListener('error', listener);
        return disposable_1.Disposable.create(() => this.socket.removeEventListener('error', listener));
    }
    onEnd(listener) {
        this.socket.addEventListener('end', listener);
        return disposable_1.Disposable.create(() => this.socket.removeEventListener('end', listener));
    }
    onData(listener) {
        return this._onData.event(listener);
    }
}
class WritableStreamWrapper {
    constructor(socket) {
        this.socket = socket;
    }
    onClose(listener) {
        this.socket.addEventListener('close', listener);
        return disposable_1.Disposable.create(() => this.socket.removeEventListener('close', listener));
    }
    onError(listener) {
        this.socket.addEventListener('error', listener);
        return disposable_1.Disposable.create(() => this.socket.removeEventListener('error', listener));
    }
    onEnd(listener) {
        this.socket.addEventListener('end', listener);
        return disposable_1.Disposable.create(() => this.socket.removeEventListener('end', listener));
    }
    write(data, encoding) {
        if (typeof data === 'string') {
            if (encoding !== undefined && encoding !== 'utf-8') {
                throw new Error(`In a Browser environments only utf-8 text encding is supported. But got encoding: ${encoding}`);
            }
            this.socket.send(data);
        }
        else {
            this.socket.send(data);
        }
        return Promise.resolve();
    }
    end() {
        this.socket.close();
    }
}
const _textEncoder = new TextEncoder();
const _ril = Object.freeze({
    messageBuffer: Object.freeze({
        create: (encoding) => new MessageBuffer(encoding)
    }),
    applicationJson: Object.freeze({
        encoder: Object.freeze({
            name: 'application/json',
            encode: (msg, options) => {
                if (options.charset !== 'utf-8') {
                    throw new Error(`In a Browser environments only utf-8 text encding is supported. But got encoding: ${options.charset}`);
                }
                return Promise.resolve(_textEncoder.encode(JSON.stringify(msg, undefined, 0)));
            }
        }),
        decoder: Object.freeze({
            name: 'application/json',
            decode: (buffer, options) => {
                if (!(buffer instanceof Uint8Array)) {
                    throw new Error(`In a Browser environments only Uint8Arrays are supported.`);
                }
                return Promise.resolve(JSON.parse(new TextDecoder(options.charset).decode(buffer)));
            }
        })
    }),
    stream: Object.freeze({
        asReadableStream: (socket) => new ReadableStreamWrapper(socket),
        asWritableStream: (socket) => new WritableStreamWrapper(socket)
    }),
    console: console,
    timer: Object.freeze({
        setTimeout(callback, ms, ...args) {
            return setTimeout(callback, ms, ...args);
        },
        clearTimeout(handle) {
            clearTimeout(handle);
        },
        setImmediate(callback, ...args) {
            return setTimeout(callback, 0, ...args);
        },
        clearImmediate(handle) {
            clearTimeout(handle);
        }
    })
});
function RIL() {
    return _ril;
}
(function (RIL) {
    function install() {
        ral_1.default.install(_ril);
    }
    RIL.install = install;
})(RIL || (RIL = {}));
exports.default = RIL;
//# sourceMappingURL=ril.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/api.js":
/*!******************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/api.js ***!
  \******************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
/// <reference path="../../typings/thenable.d.ts" />
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.CancellationSenderStrategy = exports.CancellationReceiverStrategy = exports.ConnectionError = exports.ConnectionErrors = exports.LogTraceNotification = exports.SetTraceNotification = exports.TraceFormat = exports.Trace = exports.ProgressType = exports.createMessageConnection = exports.NullLogger = exports.ConnectionOptions = exports.ConnectionStrategy = exports.WriteableStreamMessageWriter = exports.AbstractMessageWriter = exports.MessageWriter = exports.ReadableStreamMessageReader = exports.AbstractMessageReader = exports.MessageReader = exports.CancellationToken = exports.CancellationTokenSource = exports.Emitter = exports.Event = exports.Disposable = exports.ParameterStructures = exports.NotificationType9 = exports.NotificationType8 = exports.NotificationType7 = exports.NotificationType6 = exports.NotificationType5 = exports.NotificationType4 = exports.NotificationType3 = exports.NotificationType2 = exports.NotificationType1 = exports.NotificationType0 = exports.NotificationType = exports.ErrorCodes = exports.ResponseError = exports.RequestType9 = exports.RequestType8 = exports.RequestType7 = exports.RequestType6 = exports.RequestType5 = exports.RequestType4 = exports.RequestType3 = exports.RequestType2 = exports.RequestType1 = exports.RequestType0 = exports.RequestType = exports.RAL = void 0;
exports.CancellationStrategy = void 0;
const messages_1 = __webpack_require__(/*! ../common/messages */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messages.js");
Object.defineProperty(exports, "RequestType", ({ enumerable: true, get: function () { return messages_1.RequestType; } }));
Object.defineProperty(exports, "RequestType0", ({ enumerable: true, get: function () { return messages_1.RequestType0; } }));
Object.defineProperty(exports, "RequestType1", ({ enumerable: true, get: function () { return messages_1.RequestType1; } }));
Object.defineProperty(exports, "RequestType2", ({ enumerable: true, get: function () { return messages_1.RequestType2; } }));
Object.defineProperty(exports, "RequestType3", ({ enumerable: true, get: function () { return messages_1.RequestType3; } }));
Object.defineProperty(exports, "RequestType4", ({ enumerable: true, get: function () { return messages_1.RequestType4; } }));
Object.defineProperty(exports, "RequestType5", ({ enumerable: true, get: function () { return messages_1.RequestType5; } }));
Object.defineProperty(exports, "RequestType6", ({ enumerable: true, get: function () { return messages_1.RequestType6; } }));
Object.defineProperty(exports, "RequestType7", ({ enumerable: true, get: function () { return messages_1.RequestType7; } }));
Object.defineProperty(exports, "RequestType8", ({ enumerable: true, get: function () { return messages_1.RequestType8; } }));
Object.defineProperty(exports, "RequestType9", ({ enumerable: true, get: function () { return messages_1.RequestType9; } }));
Object.defineProperty(exports, "ResponseError", ({ enumerable: true, get: function () { return messages_1.ResponseError; } }));
Object.defineProperty(exports, "ErrorCodes", ({ enumerable: true, get: function () { return messages_1.ErrorCodes; } }));
Object.defineProperty(exports, "NotificationType", ({ enumerable: true, get: function () { return messages_1.NotificationType; } }));
Object.defineProperty(exports, "NotificationType0", ({ enumerable: true, get: function () { return messages_1.NotificationType0; } }));
Object.defineProperty(exports, "NotificationType1", ({ enumerable: true, get: function () { return messages_1.NotificationType1; } }));
Object.defineProperty(exports, "NotificationType2", ({ enumerable: true, get: function () { return messages_1.NotificationType2; } }));
Object.defineProperty(exports, "NotificationType3", ({ enumerable: true, get: function () { return messages_1.NotificationType3; } }));
Object.defineProperty(exports, "NotificationType4", ({ enumerable: true, get: function () { return messages_1.NotificationType4; } }));
Object.defineProperty(exports, "NotificationType5", ({ enumerable: true, get: function () { return messages_1.NotificationType5; } }));
Object.defineProperty(exports, "NotificationType6", ({ enumerable: true, get: function () { return messages_1.NotificationType6; } }));
Object.defineProperty(exports, "NotificationType7", ({ enumerable: true, get: function () { return messages_1.NotificationType7; } }));
Object.defineProperty(exports, "NotificationType8", ({ enumerable: true, get: function () { return messages_1.NotificationType8; } }));
Object.defineProperty(exports, "NotificationType9", ({ enumerable: true, get: function () { return messages_1.NotificationType9; } }));
Object.defineProperty(exports, "ParameterStructures", ({ enumerable: true, get: function () { return messages_1.ParameterStructures; } }));
const disposable_1 = __webpack_require__(/*! ../common/disposable */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/disposable.js");
Object.defineProperty(exports, "Disposable", ({ enumerable: true, get: function () { return disposable_1.Disposable; } }));
const events_1 = __webpack_require__(/*! ../common/events */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/events.js");
Object.defineProperty(exports, "Event", ({ enumerable: true, get: function () { return events_1.Event; } }));
Object.defineProperty(exports, "Emitter", ({ enumerable: true, get: function () { return events_1.Emitter; } }));
const cancellation_1 = __webpack_require__(/*! ../common/cancellation */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/cancellation.js");
Object.defineProperty(exports, "CancellationTokenSource", ({ enumerable: true, get: function () { return cancellation_1.CancellationTokenSource; } }));
Object.defineProperty(exports, "CancellationToken", ({ enumerable: true, get: function () { return cancellation_1.CancellationToken; } }));
const messageReader_1 = __webpack_require__(/*! ../common/messageReader */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messageReader.js");
Object.defineProperty(exports, "MessageReader", ({ enumerable: true, get: function () { return messageReader_1.MessageReader; } }));
Object.defineProperty(exports, "AbstractMessageReader", ({ enumerable: true, get: function () { return messageReader_1.AbstractMessageReader; } }));
Object.defineProperty(exports, "ReadableStreamMessageReader", ({ enumerable: true, get: function () { return messageReader_1.ReadableStreamMessageReader; } }));
const messageWriter_1 = __webpack_require__(/*! ../common/messageWriter */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messageWriter.js");
Object.defineProperty(exports, "MessageWriter", ({ enumerable: true, get: function () { return messageWriter_1.MessageWriter; } }));
Object.defineProperty(exports, "AbstractMessageWriter", ({ enumerable: true, get: function () { return messageWriter_1.AbstractMessageWriter; } }));
Object.defineProperty(exports, "WriteableStreamMessageWriter", ({ enumerable: true, get: function () { return messageWriter_1.WriteableStreamMessageWriter; } }));
const connection_1 = __webpack_require__(/*! ../common/connection */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/connection.js");
Object.defineProperty(exports, "ConnectionStrategy", ({ enumerable: true, get: function () { return connection_1.ConnectionStrategy; } }));
Object.defineProperty(exports, "ConnectionOptions", ({ enumerable: true, get: function () { return connection_1.ConnectionOptions; } }));
Object.defineProperty(exports, "NullLogger", ({ enumerable: true, get: function () { return connection_1.NullLogger; } }));
Object.defineProperty(exports, "createMessageConnection", ({ enumerable: true, get: function () { return connection_1.createMessageConnection; } }));
Object.defineProperty(exports, "ProgressType", ({ enumerable: true, get: function () { return connection_1.ProgressType; } }));
Object.defineProperty(exports, "Trace", ({ enumerable: true, get: function () { return connection_1.Trace; } }));
Object.defineProperty(exports, "TraceFormat", ({ enumerable: true, get: function () { return connection_1.TraceFormat; } }));
Object.defineProperty(exports, "SetTraceNotification", ({ enumerable: true, get: function () { return connection_1.SetTraceNotification; } }));
Object.defineProperty(exports, "LogTraceNotification", ({ enumerable: true, get: function () { return connection_1.LogTraceNotification; } }));
Object.defineProperty(exports, "ConnectionErrors", ({ enumerable: true, get: function () { return connection_1.ConnectionErrors; } }));
Object.defineProperty(exports, "ConnectionError", ({ enumerable: true, get: function () { return connection_1.ConnectionError; } }));
Object.defineProperty(exports, "CancellationReceiverStrategy", ({ enumerable: true, get: function () { return connection_1.CancellationReceiverStrategy; } }));
Object.defineProperty(exports, "CancellationSenderStrategy", ({ enumerable: true, get: function () { return connection_1.CancellationSenderStrategy; } }));
Object.defineProperty(exports, "CancellationStrategy", ({ enumerable: true, get: function () { return connection_1.CancellationStrategy; } }));
const ral_1 = __webpack_require__(/*! ./ral */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/ral.js");
exports.RAL = ral_1.default;
//# sourceMappingURL=api.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/cancellation.js":
/*!***************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/cancellation.js ***!
  \***************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.CancellationTokenSource = exports.CancellationToken = void 0;
const ral_1 = __webpack_require__(/*! ./ral */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/ral.js");
const Is = __webpack_require__(/*! ./is */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/is.js");
const events_1 = __webpack_require__(/*! ./events */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/events.js");
var CancellationToken;
(function (CancellationToken) {
    CancellationToken.None = Object.freeze({
        isCancellationRequested: false,
        onCancellationRequested: events_1.Event.None
    });
    CancellationToken.Cancelled = Object.freeze({
        isCancellationRequested: true,
        onCancellationRequested: events_1.Event.None
    });
    function is(value) {
        const candidate = value;
        return candidate && (candidate === CancellationToken.None
            || candidate === CancellationToken.Cancelled
            || (Is.boolean(candidate.isCancellationRequested) && !!candidate.onCancellationRequested));
    }
    CancellationToken.is = is;
})(CancellationToken = exports.CancellationToken || (exports.CancellationToken = {}));
const shortcutEvent = Object.freeze(function (callback, context) {
    const handle = ral_1.default().timer.setTimeout(callback.bind(context), 0);
    return { dispose() { ral_1.default().timer.clearTimeout(handle); } };
});
class MutableToken {
    constructor() {
        this._isCancelled = false;
    }
    cancel() {
        if (!this._isCancelled) {
            this._isCancelled = true;
            if (this._emitter) {
                this._emitter.fire(undefined);
                this.dispose();
            }
        }
    }
    get isCancellationRequested() {
        return this._isCancelled;
    }
    get onCancellationRequested() {
        if (this._isCancelled) {
            return shortcutEvent;
        }
        if (!this._emitter) {
            this._emitter = new events_1.Emitter();
        }
        return this._emitter.event;
    }
    dispose() {
        if (this._emitter) {
            this._emitter.dispose();
            this._emitter = undefined;
        }
    }
}
class CancellationTokenSource {
    get token() {
        if (!this._token) {
            // be lazy and create the token only when
            // actually needed
            this._token = new MutableToken();
        }
        return this._token;
    }
    cancel() {
        if (!this._token) {
            // save an object by returning the default
            // cancelled token when cancellation happens
            // before someone asks for the token
            this._token = CancellationToken.Cancelled;
        }
        else {
            this._token.cancel();
        }
    }
    dispose() {
        if (!this._token) {
            // ensure to initialize with an empty token if we had none
            this._token = CancellationToken.None;
        }
        else if (this._token instanceof MutableToken) {
            // actually dispose
            this._token.dispose();
        }
    }
}
exports.CancellationTokenSource = CancellationTokenSource;
//# sourceMappingURL=cancellation.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/connection.js":
/*!*************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/connection.js ***!
  \*************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createMessageConnection = exports.ConnectionOptions = exports.CancellationStrategy = exports.CancellationSenderStrategy = exports.CancellationReceiverStrategy = exports.ConnectionStrategy = exports.ConnectionError = exports.ConnectionErrors = exports.LogTraceNotification = exports.SetTraceNotification = exports.TraceFormat = exports.Trace = exports.NullLogger = exports.ProgressType = void 0;
const ral_1 = __webpack_require__(/*! ./ral */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/ral.js");
const Is = __webpack_require__(/*! ./is */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/is.js");
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messages.js");
const linkedMap_1 = __webpack_require__(/*! ./linkedMap */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/linkedMap.js");
const events_1 = __webpack_require__(/*! ./events */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/events.js");
const cancellation_1 = __webpack_require__(/*! ./cancellation */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/cancellation.js");
var CancelNotification;
(function (CancelNotification) {
    CancelNotification.type = new messages_1.NotificationType('$/cancelRequest');
})(CancelNotification || (CancelNotification = {}));
var ProgressNotification;
(function (ProgressNotification) {
    ProgressNotification.type = new messages_1.NotificationType('$/progress');
})(ProgressNotification || (ProgressNotification = {}));
class ProgressType {
    constructor() {
    }
}
exports.ProgressType = ProgressType;
var StarRequestHandler;
(function (StarRequestHandler) {
    function is(value) {
        return Is.func(value);
    }
    StarRequestHandler.is = is;
})(StarRequestHandler || (StarRequestHandler = {}));
exports.NullLogger = Object.freeze({
    error: () => { },
    warn: () => { },
    info: () => { },
    log: () => { }
});
var Trace;
(function (Trace) {
    Trace[Trace["Off"] = 0] = "Off";
    Trace[Trace["Messages"] = 1] = "Messages";
    Trace[Trace["Verbose"] = 2] = "Verbose";
})(Trace = exports.Trace || (exports.Trace = {}));
(function (Trace) {
    function fromString(value) {
        if (!Is.string(value)) {
            return Trace.Off;
        }
        value = value.toLowerCase();
        switch (value) {
            case 'off':
                return Trace.Off;
            case 'messages':
                return Trace.Messages;
            case 'verbose':
                return Trace.Verbose;
            default:
                return Trace.Off;
        }
    }
    Trace.fromString = fromString;
    function toString(value) {
        switch (value) {
            case Trace.Off:
                return 'off';
            case Trace.Messages:
                return 'messages';
            case Trace.Verbose:
                return 'verbose';
            default:
                return 'off';
        }
    }
    Trace.toString = toString;
})(Trace = exports.Trace || (exports.Trace = {}));
var TraceFormat;
(function (TraceFormat) {
    TraceFormat["Text"] = "text";
    TraceFormat["JSON"] = "json";
})(TraceFormat = exports.TraceFormat || (exports.TraceFormat = {}));
(function (TraceFormat) {
    function fromString(value) {
        value = value.toLowerCase();
        if (value === 'json') {
            return TraceFormat.JSON;
        }
        else {
            return TraceFormat.Text;
        }
    }
    TraceFormat.fromString = fromString;
})(TraceFormat = exports.TraceFormat || (exports.TraceFormat = {}));
var SetTraceNotification;
(function (SetTraceNotification) {
    SetTraceNotification.type = new messages_1.NotificationType('$/setTrace');
})(SetTraceNotification = exports.SetTraceNotification || (exports.SetTraceNotification = {}));
var LogTraceNotification;
(function (LogTraceNotification) {
    LogTraceNotification.type = new messages_1.NotificationType('$/logTrace');
})(LogTraceNotification = exports.LogTraceNotification || (exports.LogTraceNotification = {}));
var ConnectionErrors;
(function (ConnectionErrors) {
    /**
     * The connection is closed.
     */
    ConnectionErrors[ConnectionErrors["Closed"] = 1] = "Closed";
    /**
     * The connection got disposed.
     */
    ConnectionErrors[ConnectionErrors["Disposed"] = 2] = "Disposed";
    /**
     * The connection is already in listening mode.
     */
    ConnectionErrors[ConnectionErrors["AlreadyListening"] = 3] = "AlreadyListening";
})(ConnectionErrors = exports.ConnectionErrors || (exports.ConnectionErrors = {}));
class ConnectionError extends Error {
    constructor(code, message) {
        super(message);
        this.code = code;
        Object.setPrototypeOf(this, ConnectionError.prototype);
    }
}
exports.ConnectionError = ConnectionError;
var ConnectionStrategy;
(function (ConnectionStrategy) {
    function is(value) {
        const candidate = value;
        return candidate && Is.func(candidate.cancelUndispatched);
    }
    ConnectionStrategy.is = is;
})(ConnectionStrategy = exports.ConnectionStrategy || (exports.ConnectionStrategy = {}));
var CancellationReceiverStrategy;
(function (CancellationReceiverStrategy) {
    CancellationReceiverStrategy.Message = Object.freeze({
        createCancellationTokenSource(_) {
            return new cancellation_1.CancellationTokenSource();
        }
    });
    function is(value) {
        const candidate = value;
        return candidate && Is.func(candidate.createCancellationTokenSource);
    }
    CancellationReceiverStrategy.is = is;
})(CancellationReceiverStrategy = exports.CancellationReceiverStrategy || (exports.CancellationReceiverStrategy = {}));
var CancellationSenderStrategy;
(function (CancellationSenderStrategy) {
    CancellationSenderStrategy.Message = Object.freeze({
        sendCancellation(conn, id) {
            conn.sendNotification(CancelNotification.type, { id });
        },
        cleanup(_) { }
    });
    function is(value) {
        const candidate = value;
        return candidate && Is.func(candidate.sendCancellation) && Is.func(candidate.cleanup);
    }
    CancellationSenderStrategy.is = is;
})(CancellationSenderStrategy = exports.CancellationSenderStrategy || (exports.CancellationSenderStrategy = {}));
var CancellationStrategy;
(function (CancellationStrategy) {
    CancellationStrategy.Message = Object.freeze({
        receiver: CancellationReceiverStrategy.Message,
        sender: CancellationSenderStrategy.Message
    });
    function is(value) {
        const candidate = value;
        return candidate && CancellationReceiverStrategy.is(candidate.receiver) && CancellationSenderStrategy.is(candidate.sender);
    }
    CancellationStrategy.is = is;
})(CancellationStrategy = exports.CancellationStrategy || (exports.CancellationStrategy = {}));
var ConnectionOptions;
(function (ConnectionOptions) {
    function is(value) {
        const candidate = value;
        return candidate && (CancellationStrategy.is(candidate.cancellationStrategy) || ConnectionStrategy.is(candidate.connectionStrategy));
    }
    ConnectionOptions.is = is;
})(ConnectionOptions = exports.ConnectionOptions || (exports.ConnectionOptions = {}));
var ConnectionState;
(function (ConnectionState) {
    ConnectionState[ConnectionState["New"] = 1] = "New";
    ConnectionState[ConnectionState["Listening"] = 2] = "Listening";
    ConnectionState[ConnectionState["Closed"] = 3] = "Closed";
    ConnectionState[ConnectionState["Disposed"] = 4] = "Disposed";
})(ConnectionState || (ConnectionState = {}));
function createMessageConnection(messageReader, messageWriter, _logger, options) {
    const logger = _logger !== undefined ? _logger : exports.NullLogger;
    let sequenceNumber = 0;
    let notificationSquenceNumber = 0;
    let unknownResponseSquenceNumber = 0;
    const version = '2.0';
    let starRequestHandler = undefined;
    const requestHandlers = Object.create(null);
    let starNotificationHandler = undefined;
    const notificationHandlers = Object.create(null);
    const progressHandlers = new Map();
    let timer;
    let messageQueue = new linkedMap_1.LinkedMap();
    let responsePromises = Object.create(null);
    let requestTokens = Object.create(null);
    let trace = Trace.Off;
    let traceFormat = TraceFormat.Text;
    let tracer;
    let state = ConnectionState.New;
    const errorEmitter = new events_1.Emitter();
    const closeEmitter = new events_1.Emitter();
    const unhandledNotificationEmitter = new events_1.Emitter();
    const unhandledProgressEmitter = new events_1.Emitter();
    const disposeEmitter = new events_1.Emitter();
    const cancellationStrategy = (options && options.cancellationStrategy) ? options.cancellationStrategy : CancellationStrategy.Message;
    function createRequestQueueKey(id) {
        if (id === null) {
            throw new Error(`Can't send requests with id null since the response can't be correlated.`);
        }
        return 'req-' + id.toString();
    }
    function createResponseQueueKey(id) {
        if (id === null) {
            return 'res-unknown-' + (++unknownResponseSquenceNumber).toString();
        }
        else {
            return 'res-' + id.toString();
        }
    }
    function createNotificationQueueKey() {
        return 'not-' + (++notificationSquenceNumber).toString();
    }
    function addMessageToQueue(queue, message) {
        if (messages_1.isRequestMessage(message)) {
            queue.set(createRequestQueueKey(message.id), message);
        }
        else if (messages_1.isResponseMessage(message)) {
            queue.set(createResponseQueueKey(message.id), message);
        }
        else {
            queue.set(createNotificationQueueKey(), message);
        }
    }
    function cancelUndispatched(_message) {
        return undefined;
    }
    function isListening() {
        return state === ConnectionState.Listening;
    }
    function isClosed() {
        return state === ConnectionState.Closed;
    }
    function isDisposed() {
        return state === ConnectionState.Disposed;
    }
    function closeHandler() {
        if (state === ConnectionState.New || state === ConnectionState.Listening) {
            state = ConnectionState.Closed;
            closeEmitter.fire(undefined);
        }
        // If the connection is disposed don't sent close events.
    }
    function readErrorHandler(error) {
        errorEmitter.fire([error, undefined, undefined]);
    }
    function writeErrorHandler(data) {
        errorEmitter.fire(data);
    }
    messageReader.onClose(closeHandler);
    messageReader.onError(readErrorHandler);
    messageWriter.onClose(closeHandler);
    messageWriter.onError(writeErrorHandler);
    function triggerMessageQueue() {
        if (timer || messageQueue.size === 0) {
            return;
        }
        timer = ral_1.default().timer.setImmediate(() => {
            timer = undefined;
            processMessageQueue();
        });
    }
    function processMessageQueue() {
        if (messageQueue.size === 0) {
            return;
        }
        const message = messageQueue.shift();
        try {
            if (messages_1.isRequestMessage(message)) {
                handleRequest(message);
            }
            else if (messages_1.isNotificationMessage(message)) {
                handleNotification(message);
            }
            else if (messages_1.isResponseMessage(message)) {
                handleResponse(message);
            }
            else {
                handleInvalidMessage(message);
            }
        }
        finally {
            triggerMessageQueue();
        }
    }
    const callback = (message) => {
        try {
            // We have received a cancellation message. Check if the message is still in the queue
            // and cancel it if allowed to do so.
            if (messages_1.isNotificationMessage(message) && message.method === CancelNotification.type.method) {
                const key = createRequestQueueKey(message.params.id);
                const toCancel = messageQueue.get(key);
                if (messages_1.isRequestMessage(toCancel)) {
                    const strategy = options === null || options === void 0 ? void 0 : options.connectionStrategy;
                    const response = (strategy && strategy.cancelUndispatched) ? strategy.cancelUndispatched(toCancel, cancelUndispatched) : cancelUndispatched(toCancel);
                    if (response && (response.error !== undefined || response.result !== undefined)) {
                        messageQueue.delete(key);
                        response.id = toCancel.id;
                        traceSendingResponse(response, message.method, Date.now());
                        messageWriter.write(response);
                        return;
                    }
                }
            }
            addMessageToQueue(messageQueue, message);
        }
        finally {
            triggerMessageQueue();
        }
    };
    function handleRequest(requestMessage) {
        if (isDisposed()) {
            // we return here silently since we fired an event when the
            // connection got disposed.
            return;
        }
        function reply(resultOrError, method, startTime) {
            const message = {
                jsonrpc: version,
                id: requestMessage.id
            };
            if (resultOrError instanceof messages_1.ResponseError) {
                message.error = resultOrError.toJson();
            }
            else {
                message.result = resultOrError === undefined ? null : resultOrError;
            }
            traceSendingResponse(message, method, startTime);
            messageWriter.write(message);
        }
        function replyError(error, method, startTime) {
            const message = {
                jsonrpc: version,
                id: requestMessage.id,
                error: error.toJson()
            };
            traceSendingResponse(message, method, startTime);
            messageWriter.write(message);
        }
        function replySuccess(result, method, startTime) {
            // The JSON RPC defines that a response must either have a result or an error
            // So we can't treat undefined as a valid response result.
            if (result === undefined) {
                result = null;
            }
            const message = {
                jsonrpc: version,
                id: requestMessage.id,
                result: result
            };
            traceSendingResponse(message, method, startTime);
            messageWriter.write(message);
        }
        traceReceivedRequest(requestMessage);
        const element = requestHandlers[requestMessage.method];
        let type;
        let requestHandler;
        if (element) {
            type = element.type;
            requestHandler = element.handler;
        }
        const startTime = Date.now();
        if (requestHandler || starRequestHandler) {
            const tokenKey = String(requestMessage.id);
            const cancellationSource = cancellationStrategy.receiver.createCancellationTokenSource(tokenKey);
            requestTokens[tokenKey] = cancellationSource;
            try {
                let handlerResult;
                if (requestHandler) {
                    if (requestMessage.params === undefined) {
                        if (type !== undefined && type.numberOfParams !== 0) {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InvalidParams, `Request ${requestMessage.method} defines ${type.numberOfParams} params but recevied none.`), requestMessage.method, startTime);
                            return;
                        }
                        handlerResult = requestHandler(cancellationSource.token);
                    }
                    else if (Array.isArray(requestMessage.params)) {
                        if (type !== undefined && type.parameterStructures === messages_1.ParameterStructures.byName) {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InvalidParams, `Request ${requestMessage.method} defines parameters by name but received parameters by position`), requestMessage.method, startTime);
                            return;
                        }
                        handlerResult = requestHandler(...requestMessage.params, cancellationSource.token);
                    }
                    else {
                        if (type !== undefined && type.parameterStructures === messages_1.ParameterStructures.byPosition) {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InvalidParams, `Request ${requestMessage.method} defines parameters by position but received parameters by name`), requestMessage.method, startTime);
                            return;
                        }
                        handlerResult = requestHandler(requestMessage.params, cancellationSource.token);
                    }
                }
                else if (starRequestHandler) {
                    handlerResult = starRequestHandler(requestMessage.method, requestMessage.params, cancellationSource.token);
                }
                const promise = handlerResult;
                if (!handlerResult) {
                    delete requestTokens[tokenKey];
                    replySuccess(handlerResult, requestMessage.method, startTime);
                }
                else if (promise.then) {
                    promise.then((resultOrError) => {
                        delete requestTokens[tokenKey];
                        reply(resultOrError, requestMessage.method, startTime);
                    }, error => {
                        delete requestTokens[tokenKey];
                        if (error instanceof messages_1.ResponseError) {
                            replyError(error, requestMessage.method, startTime);
                        }
                        else if (error && Is.string(error.message)) {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed with message: ${error.message}`), requestMessage.method, startTime);
                        }
                        else {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed unexpectedly without providing any details.`), requestMessage.method, startTime);
                        }
                    });
                }
                else {
                    delete requestTokens[tokenKey];
                    reply(handlerResult, requestMessage.method, startTime);
                }
            }
            catch (error) {
                delete requestTokens[tokenKey];
                if (error instanceof messages_1.ResponseError) {
                    reply(error, requestMessage.method, startTime);
                }
                else if (error && Is.string(error.message)) {
                    replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed with message: ${error.message}`), requestMessage.method, startTime);
                }
                else {
                    replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed unexpectedly without providing any details.`), requestMessage.method, startTime);
                }
            }
        }
        else {
            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.MethodNotFound, `Unhandled method ${requestMessage.method}`), requestMessage.method, startTime);
        }
    }
    function handleResponse(responseMessage) {
        if (isDisposed()) {
            // See handle request.
            return;
        }
        if (responseMessage.id === null) {
            if (responseMessage.error) {
                logger.error(`Received response message without id: Error is: \n${JSON.stringify(responseMessage.error, undefined, 4)}`);
            }
            else {
                logger.error(`Received response message without id. No further error information provided.`);
            }
        }
        else {
            const key = String(responseMessage.id);
            const responsePromise = responsePromises[key];
            traceReceivedResponse(responseMessage, responsePromise);
            if (responsePromise) {
                delete responsePromises[key];
                try {
                    if (responseMessage.error) {
                        const error = responseMessage.error;
                        responsePromise.reject(new messages_1.ResponseError(error.code, error.message, error.data));
                    }
                    else if (responseMessage.result !== undefined) {
                        responsePromise.resolve(responseMessage.result);
                    }
                    else {
                        throw new Error('Should never happen.');
                    }
                }
                catch (error) {
                    if (error.message) {
                        logger.error(`Response handler '${responsePromise.method}' failed with message: ${error.message}`);
                    }
                    else {
                        logger.error(`Response handler '${responsePromise.method}' failed unexpectedly.`);
                    }
                }
            }
        }
    }
    function handleNotification(message) {
        if (isDisposed()) {
            // See handle request.
            return;
        }
        let type = undefined;
        let notificationHandler;
        if (message.method === CancelNotification.type.method) {
            notificationHandler = (params) => {
                const id = params.id;
                const source = requestTokens[String(id)];
                if (source) {
                    source.cancel();
                }
            };
        }
        else {
            const element = notificationHandlers[message.method];
            if (element) {
                notificationHandler = element.handler;
                type = element.type;
            }
        }
        if (notificationHandler || starNotificationHandler) {
            try {
                traceReceivedNotification(message);
                if (notificationHandler) {
                    if (message.params === undefined) {
                        if (type !== undefined) {
                            if (type.numberOfParams !== 0 && type.parameterStructures !== messages_1.ParameterStructures.byName) {
                                logger.error(`Notification ${message.method} defines ${type.numberOfParams} params but recevied none.`);
                            }
                        }
                        notificationHandler();
                    }
                    else if (Array.isArray(message.params)) {
                        if (type !== undefined) {
                            if (type.parameterStructures === messages_1.ParameterStructures.byName) {
                                logger.error(`Notification ${message.method} defines parameters by name but received parameters by position`);
                            }
                            if (type.numberOfParams !== message.params.length) {
                                logger.error(`Notification ${message.method} defines ${type.numberOfParams} params but received ${message.params.length} argumennts`);
                            }
                        }
                        notificationHandler(...message.params);
                    }
                    else {
                        if (type !== undefined && type.parameterStructures === messages_1.ParameterStructures.byPosition) {
                            logger.error(`Notification ${message.method} defines parameters by position but received parameters by name`);
                        }
                        notificationHandler(message.params);
                    }
                }
                else if (starNotificationHandler) {
                    starNotificationHandler(message.method, message.params);
                }
            }
            catch (error) {
                if (error.message) {
                    logger.error(`Notification handler '${message.method}' failed with message: ${error.message}`);
                }
                else {
                    logger.error(`Notification handler '${message.method}' failed unexpectedly.`);
                }
            }
        }
        else {
            unhandledNotificationEmitter.fire(message);
        }
    }
    function handleInvalidMessage(message) {
        if (!message) {
            logger.error('Received empty message.');
            return;
        }
        logger.error(`Received message which is neither a response nor a notification message:\n${JSON.stringify(message, null, 4)}`);
        // Test whether we find an id to reject the promise
        const responseMessage = message;
        if (Is.string(responseMessage.id) || Is.number(responseMessage.id)) {
            const key = String(responseMessage.id);
            const responseHandler = responsePromises[key];
            if (responseHandler) {
                responseHandler.reject(new Error('The received response has neither a result nor an error property.'));
            }
        }
    }
    function traceSendingRequest(message) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose && message.params) {
                data = `Params: ${JSON.stringify(message.params, null, 4)}\n\n`;
            }
            tracer.log(`Sending request '${message.method} - (${message.id})'.`, data);
        }
        else {
            logLSPMessage('send-request', message);
        }
    }
    function traceSendingNotification(message) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose) {
                if (message.params) {
                    data = `Params: ${JSON.stringify(message.params, null, 4)}\n\n`;
                }
                else {
                    data = 'No parameters provided.\n\n';
                }
            }
            tracer.log(`Sending notification '${message.method}'.`, data);
        }
        else {
            logLSPMessage('send-notification', message);
        }
    }
    function traceSendingResponse(message, method, startTime) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose) {
                if (message.error && message.error.data) {
                    data = `Error data: ${JSON.stringify(message.error.data, null, 4)}\n\n`;
                }
                else {
                    if (message.result) {
                        data = `Result: ${JSON.stringify(message.result, null, 4)}\n\n`;
                    }
                    else if (message.error === undefined) {
                        data = 'No result returned.\n\n';
                    }
                }
            }
            tracer.log(`Sending response '${method} - (${message.id})'. Processing request took ${Date.now() - startTime}ms`, data);
        }
        else {
            logLSPMessage('send-response', message);
        }
    }
    function traceReceivedRequest(message) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose && message.params) {
                data = `Params: ${JSON.stringify(message.params, null, 4)}\n\n`;
            }
            tracer.log(`Received request '${message.method} - (${message.id})'.`, data);
        }
        else {
            logLSPMessage('receive-request', message);
        }
    }
    function traceReceivedNotification(message) {
        if (trace === Trace.Off || !tracer || message.method === LogTraceNotification.type.method) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose) {
                if (message.params) {
                    data = `Params: ${JSON.stringify(message.params, null, 4)}\n\n`;
                }
                else {
                    data = 'No parameters provided.\n\n';
                }
            }
            tracer.log(`Received notification '${message.method}'.`, data);
        }
        else {
            logLSPMessage('receive-notification', message);
        }
    }
    function traceReceivedResponse(message, responsePromise) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose) {
                if (message.error && message.error.data) {
                    data = `Error data: ${JSON.stringify(message.error.data, null, 4)}\n\n`;
                }
                else {
                    if (message.result) {
                        data = `Result: ${JSON.stringify(message.result, null, 4)}\n\n`;
                    }
                    else if (message.error === undefined) {
                        data = 'No result returned.\n\n';
                    }
                }
            }
            if (responsePromise) {
                const error = message.error ? ` Request failed: ${message.error.message} (${message.error.code}).` : '';
                tracer.log(`Received response '${responsePromise.method} - (${message.id})' in ${Date.now() - responsePromise.timerStart}ms.${error}`, data);
            }
            else {
                tracer.log(`Received response ${message.id} without active response promise.`, data);
            }
        }
        else {
            logLSPMessage('receive-response', message);
        }
    }
    function logLSPMessage(type, message) {
        if (!tracer || trace === Trace.Off) {
            return;
        }
        const lspMessage = {
            isLSPMessage: true,
            type,
            message,
            timestamp: Date.now()
        };
        tracer.log(lspMessage);
    }
    function throwIfClosedOrDisposed() {
        if (isClosed()) {
            throw new ConnectionError(ConnectionErrors.Closed, 'Connection is closed.');
        }
        if (isDisposed()) {
            throw new ConnectionError(ConnectionErrors.Disposed, 'Connection is disposed.');
        }
    }
    function throwIfListening() {
        if (isListening()) {
            throw new ConnectionError(ConnectionErrors.AlreadyListening, 'Connection is already listening');
        }
    }
    function throwIfNotListening() {
        if (!isListening()) {
            throw new Error('Call listen() first.');
        }
    }
    function undefinedToNull(param) {
        if (param === undefined) {
            return null;
        }
        else {
            return param;
        }
    }
    function nullToUndefined(param) {
        if (param === null) {
            return undefined;
        }
        else {
            return param;
        }
    }
    function isNamedParam(param) {
        return param !== undefined && param !== null && !Array.isArray(param) && typeof param === 'object';
    }
    function computeSingleParam(parameterStructures, param) {
        switch (parameterStructures) {
            case messages_1.ParameterStructures.auto:
                if (isNamedParam(param)) {
                    return nullToUndefined(param);
                }
                else {
                    return [undefinedToNull(param)];
                }
                break;
            case messages_1.ParameterStructures.byName:
                if (!isNamedParam(param)) {
                    throw new Error(`Recevied parameters by name but param is not an object literal.`);
                }
                return nullToUndefined(param);
            case messages_1.ParameterStructures.byPosition:
                return [undefinedToNull(param)];
            default:
                throw new Error(`Unknown parameter structure ${parameterStructures.toString()}`);
        }
    }
    function computeMessageParams(type, params) {
        let result;
        const numberOfParams = type.numberOfParams;
        switch (numberOfParams) {
            case 0:
                result = undefined;
                break;
            case 1:
                result = computeSingleParam(type.parameterStructures, params[0]);
                break;
            default:
                result = [];
                for (let i = 0; i < params.length && i < numberOfParams; i++) {
                    result.push(undefinedToNull(params[i]));
                }
                if (params.length < numberOfParams) {
                    for (let i = params.length; i < numberOfParams; i++) {
                        result.push(null);
                    }
                }
                break;
        }
        return result;
    }
    const connection = {
        sendNotification: (type, ...args) => {
            throwIfClosedOrDisposed();
            let method;
            let messageParams;
            if (Is.string(type)) {
                method = type;
                const first = args[0];
                let paramStart = 0;
                let parameterStructures = messages_1.ParameterStructures.auto;
                if (messages_1.ParameterStructures.is(first)) {
                    paramStart = 1;
                    parameterStructures = first;
                }
                let paramEnd = args.length;
                const numberOfParams = paramEnd - paramStart;
                switch (numberOfParams) {
                    case 0:
                        messageParams = undefined;
                        break;
                    case 1:
                        messageParams = computeSingleParam(parameterStructures, args[paramStart]);
                        break;
                    default:
                        if (parameterStructures === messages_1.ParameterStructures.byName) {
                            throw new Error(`Recevied ${numberOfParams} parameters for 'by Name' notification parameter structure.`);
                        }
                        messageParams = args.slice(paramStart, paramEnd).map(value => undefinedToNull(value));
                        break;
                }
            }
            else {
                const params = args;
                method = type.method;
                messageParams = computeMessageParams(type, params);
            }
            const notificationMessage = {
                jsonrpc: version,
                method: method,
                params: messageParams
            };
            traceSendingNotification(notificationMessage);
            messageWriter.write(notificationMessage);
        },
        onNotification: (type, handler) => {
            throwIfClosedOrDisposed();
            let method;
            if (Is.func(type)) {
                starNotificationHandler = type;
            }
            else if (handler) {
                if (Is.string(type)) {
                    method = type;
                    notificationHandlers[type] = { type: undefined, handler };
                }
                else {
                    method = type.method;
                    notificationHandlers[type.method] = { type, handler };
                }
            }
            return {
                dispose: () => {
                    if (method !== undefined) {
                        delete notificationHandlers[method];
                    }
                    else {
                        starNotificationHandler = undefined;
                    }
                }
            };
        },
        onProgress: (_type, token, handler) => {
            if (progressHandlers.has(token)) {
                throw new Error(`Progress handler for token ${token} already registered`);
            }
            progressHandlers.set(token, handler);
            return {
                dispose: () => {
                    progressHandlers.delete(token);
                }
            };
        },
        sendProgress: (_type, token, value) => {
            connection.sendNotification(ProgressNotification.type, { token, value });
        },
        onUnhandledProgress: unhandledProgressEmitter.event,
        sendRequest: (type, ...args) => {
            throwIfClosedOrDisposed();
            throwIfNotListening();
            let method;
            let messageParams;
            let token = undefined;
            if (Is.string(type)) {
                method = type;
                const first = args[0];
                const last = args[args.length - 1];
                let paramStart = 0;
                let parameterStructures = messages_1.ParameterStructures.auto;
                if (messages_1.ParameterStructures.is(first)) {
                    paramStart = 1;
                    parameterStructures = first;
                }
                let paramEnd = args.length;
                if (cancellation_1.CancellationToken.is(last)) {
                    paramEnd = paramEnd - 1;
                    token = last;
                }
                const numberOfParams = paramEnd - paramStart;
                switch (numberOfParams) {
                    case 0:
                        messageParams = undefined;
                        break;
                    case 1:
                        messageParams = computeSingleParam(parameterStructures, args[paramStart]);
                        break;
                    default:
                        if (parameterStructures === messages_1.ParameterStructures.byName) {
                            throw new Error(`Recevied ${numberOfParams} parameters for 'by Name' request parameter structure.`);
                        }
                        messageParams = args.slice(paramStart, paramEnd).map(value => undefinedToNull(value));
                        break;
                }
            }
            else {
                const params = args;
                method = type.method;
                messageParams = computeMessageParams(type, params);
                const numberOfParams = type.numberOfParams;
                token = cancellation_1.CancellationToken.is(params[numberOfParams]) ? params[numberOfParams] : undefined;
            }
            const id = sequenceNumber++;
            let disposable;
            if (token) {
                disposable = token.onCancellationRequested(() => {
                    cancellationStrategy.sender.sendCancellation(connection, id);
                });
            }
            const result = new Promise((resolve, reject) => {
                const requestMessage = {
                    jsonrpc: version,
                    id: id,
                    method: method,
                    params: messageParams
                };
                const resolveWithCleanup = (r) => {
                    resolve(r);
                    cancellationStrategy.sender.cleanup(id);
                    disposable === null || disposable === void 0 ? void 0 : disposable.dispose();
                };
                const rejectWithCleanup = (r) => {
                    reject(r);
                    cancellationStrategy.sender.cleanup(id);
                    disposable === null || disposable === void 0 ? void 0 : disposable.dispose();
                };
                let responsePromise = { method: method, timerStart: Date.now(), resolve: resolveWithCleanup, reject: rejectWithCleanup };
                traceSendingRequest(requestMessage);
                try {
                    messageWriter.write(requestMessage);
                }
                catch (e) {
                    // Writing the message failed. So we need to reject the promise.
                    responsePromise.reject(new messages_1.ResponseError(messages_1.ErrorCodes.MessageWriteError, e.message ? e.message : 'Unknown reason'));
                    responsePromise = null;
                }
                if (responsePromise) {
                    responsePromises[String(id)] = responsePromise;
                }
            });
            return result;
        },
        onRequest: (type, handler) => {
            throwIfClosedOrDisposed();
            let method = null;
            if (StarRequestHandler.is(type)) {
                method = undefined;
                starRequestHandler = type;
            }
            else if (Is.string(type)) {
                method = null;
                if (handler !== undefined) {
                    method = type;
                    requestHandlers[type] = { handler: handler, type: undefined };
                }
            }
            else {
                if (handler !== undefined) {
                    method = type.method;
                    requestHandlers[type.method] = { type, handler };
                }
            }
            return {
                dispose: () => {
                    if (method === null) {
                        return;
                    }
                    if (method !== undefined) {
                        delete requestHandlers[method];
                    }
                    else {
                        starRequestHandler = undefined;
                    }
                }
            };
        },
        trace: (_value, _tracer, sendNotificationOrTraceOptions) => {
            let _sendNotification = false;
            let _traceFormat = TraceFormat.Text;
            if (sendNotificationOrTraceOptions !== undefined) {
                if (Is.boolean(sendNotificationOrTraceOptions)) {
                    _sendNotification = sendNotificationOrTraceOptions;
                }
                else {
                    _sendNotification = sendNotificationOrTraceOptions.sendNotification || false;
                    _traceFormat = sendNotificationOrTraceOptions.traceFormat || TraceFormat.Text;
                }
            }
            trace = _value;
            traceFormat = _traceFormat;
            if (trace === Trace.Off) {
                tracer = undefined;
            }
            else {
                tracer = _tracer;
            }
            if (_sendNotification && !isClosed() && !isDisposed()) {
                connection.sendNotification(SetTraceNotification.type, { value: Trace.toString(_value) });
            }
        },
        onError: errorEmitter.event,
        onClose: closeEmitter.event,
        onUnhandledNotification: unhandledNotificationEmitter.event,
        onDispose: disposeEmitter.event,
        end: () => {
            messageWriter.end();
        },
        dispose: () => {
            if (isDisposed()) {
                return;
            }
            state = ConnectionState.Disposed;
            disposeEmitter.fire(undefined);
            const error = new Error('Connection got disposed.');
            Object.keys(responsePromises).forEach((key) => {
                responsePromises[key].reject(error);
            });
            responsePromises = Object.create(null);
            requestTokens = Object.create(null);
            messageQueue = new linkedMap_1.LinkedMap();
            // Test for backwards compatibility
            if (Is.func(messageWriter.dispose)) {
                messageWriter.dispose();
            }
            if (Is.func(messageReader.dispose)) {
                messageReader.dispose();
            }
        },
        listen: () => {
            throwIfClosedOrDisposed();
            throwIfListening();
            state = ConnectionState.Listening;
            messageReader.listen(callback);
        },
        inspect: () => {
            // eslint-disable-next-line no-console
            ral_1.default().console.log('inspect');
        }
    };
    connection.onNotification(LogTraceNotification.type, (params) => {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        tracer.log(params.message, trace === Trace.Verbose ? params.verbose : undefined);
    });
    connection.onNotification(ProgressNotification.type, (params) => {
        const handler = progressHandlers.get(params.token);
        if (handler) {
            handler(params.value);
        }
        else {
            unhandledProgressEmitter.fire(params);
        }
    });
    return connection;
}
exports.createMessageConnection = createMessageConnection;
//# sourceMappingURL=connection.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/disposable.js":
/*!*************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/disposable.js ***!
  \*************************************************************************************/
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Disposable = void 0;
var Disposable;
(function (Disposable) {
    function create(func) {
        return {
            dispose: func
        };
    }
    Disposable.create = create;
})(Disposable = exports.Disposable || (exports.Disposable = {}));
//# sourceMappingURL=disposable.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/events.js":
/*!*********************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/events.js ***!
  \*********************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Emitter = exports.Event = void 0;
const ral_1 = __webpack_require__(/*! ./ral */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/ral.js");
var Event;
(function (Event) {
    const _disposable = { dispose() { } };
    Event.None = function () { return _disposable; };
})(Event = exports.Event || (exports.Event = {}));
class CallbackList {
    add(callback, context = null, bucket) {
        if (!this._callbacks) {
            this._callbacks = [];
            this._contexts = [];
        }
        this._callbacks.push(callback);
        this._contexts.push(context);
        if (Array.isArray(bucket)) {
            bucket.push({ dispose: () => this.remove(callback, context) });
        }
    }
    remove(callback, context = null) {
        if (!this._callbacks) {
            return;
        }
        let foundCallbackWithDifferentContext = false;
        for (let i = 0, len = this._callbacks.length; i < len; i++) {
            if (this._callbacks[i] === callback) {
                if (this._contexts[i] === context) {
                    // callback & context match => remove it
                    this._callbacks.splice(i, 1);
                    this._contexts.splice(i, 1);
                    return;
                }
                else {
                    foundCallbackWithDifferentContext = true;
                }
            }
        }
        if (foundCallbackWithDifferentContext) {
            throw new Error('When adding a listener with a context, you should remove it with the same context');
        }
    }
    invoke(...args) {
        if (!this._callbacks) {
            return [];
        }
        const ret = [], callbacks = this._callbacks.slice(0), contexts = this._contexts.slice(0);
        for (let i = 0, len = callbacks.length; i < len; i++) {
            try {
                ret.push(callbacks[i].apply(contexts[i], args));
            }
            catch (e) {
                // eslint-disable-next-line no-console
                ral_1.default().console.error(e);
            }
        }
        return ret;
    }
    isEmpty() {
        return !this._callbacks || this._callbacks.length === 0;
    }
    dispose() {
        this._callbacks = undefined;
        this._contexts = undefined;
    }
}
class Emitter {
    constructor(_options) {
        this._options = _options;
    }
    /**
     * For the public to allow to subscribe
     * to events from this Emitter
     */
    get event() {
        if (!this._event) {
            this._event = (listener, thisArgs, disposables) => {
                if (!this._callbacks) {
                    this._callbacks = new CallbackList();
                }
                if (this._options && this._options.onFirstListenerAdd && this._callbacks.isEmpty()) {
                    this._options.onFirstListenerAdd(this);
                }
                this._callbacks.add(listener, thisArgs);
                const result = {
                    dispose: () => {
                        if (!this._callbacks) {
                            // disposable is disposed after emitter is disposed.
                            return;
                        }
                        this._callbacks.remove(listener, thisArgs);
                        result.dispose = Emitter._noop;
                        if (this._options && this._options.onLastListenerRemove && this._callbacks.isEmpty()) {
                            this._options.onLastListenerRemove(this);
                        }
                    }
                };
                if (Array.isArray(disposables)) {
                    disposables.push(result);
                }
                return result;
            };
        }
        return this._event;
    }
    /**
     * To be kept private to fire an event to
     * subscribers
     */
    fire(event) {
        if (this._callbacks) {
            this._callbacks.invoke.call(this._callbacks, event);
        }
    }
    dispose() {
        if (this._callbacks) {
            this._callbacks.dispose();
            this._callbacks = undefined;
        }
    }
}
exports.Emitter = Emitter;
Emitter._noop = function () { };
//# sourceMappingURL=events.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/is.js":
/*!*****************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/is.js ***!
  \*****************************************************************************/
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.stringArray = exports.array = exports.func = exports.error = exports.number = exports.string = exports.boolean = void 0;
function boolean(value) {
    return value === true || value === false;
}
exports.boolean = boolean;
function string(value) {
    return typeof value === 'string' || value instanceof String;
}
exports.string = string;
function number(value) {
    return typeof value === 'number' || value instanceof Number;
}
exports.number = number;
function error(value) {
    return value instanceof Error;
}
exports.error = error;
function func(value) {
    return typeof value === 'function';
}
exports.func = func;
function array(value) {
    return Array.isArray(value);
}
exports.array = array;
function stringArray(value) {
    return array(value) && value.every(elem => string(elem));
}
exports.stringArray = stringArray;
//# sourceMappingURL=is.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/linkedMap.js":
/*!************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/linkedMap.js ***!
  \************************************************************************************/
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.LRUCache = exports.LinkedMap = exports.Touch = void 0;
var Touch;
(function (Touch) {
    Touch.None = 0;
    Touch.First = 1;
    Touch.AsOld = Touch.First;
    Touch.Last = 2;
    Touch.AsNew = Touch.Last;
})(Touch = exports.Touch || (exports.Touch = {}));
class LinkedMap {
    constructor() {
        this[Symbol.toStringTag] = 'LinkedMap';
        this._map = new Map();
        this._head = undefined;
        this._tail = undefined;
        this._size = 0;
        this._state = 0;
    }
    clear() {
        this._map.clear();
        this._head = undefined;
        this._tail = undefined;
        this._size = 0;
        this._state++;
    }
    isEmpty() {
        return !this._head && !this._tail;
    }
    get size() {
        return this._size;
    }
    get first() {
        var _a;
        return (_a = this._head) === null || _a === void 0 ? void 0 : _a.value;
    }
    get last() {
        var _a;
        return (_a = this._tail) === null || _a === void 0 ? void 0 : _a.value;
    }
    has(key) {
        return this._map.has(key);
    }
    get(key, touch = Touch.None) {
        const item = this._map.get(key);
        if (!item) {
            return undefined;
        }
        if (touch !== Touch.None) {
            this.touch(item, touch);
        }
        return item.value;
    }
    set(key, value, touch = Touch.None) {
        let item = this._map.get(key);
        if (item) {
            item.value = value;
            if (touch !== Touch.None) {
                this.touch(item, touch);
            }
        }
        else {
            item = { key, value, next: undefined, previous: undefined };
            switch (touch) {
                case Touch.None:
                    this.addItemLast(item);
                    break;
                case Touch.First:
                    this.addItemFirst(item);
                    break;
                case Touch.Last:
                    this.addItemLast(item);
                    break;
                default:
                    this.addItemLast(item);
                    break;
            }
            this._map.set(key, item);
            this._size++;
        }
        return this;
    }
    delete(key) {
        return !!this.remove(key);
    }
    remove(key) {
        const item = this._map.get(key);
        if (!item) {
            return undefined;
        }
        this._map.delete(key);
        this.removeItem(item);
        this._size--;
        return item.value;
    }
    shift() {
        if (!this._head && !this._tail) {
            return undefined;
        }
        if (!this._head || !this._tail) {
            throw new Error('Invalid list');
        }
        const item = this._head;
        this._map.delete(item.key);
        this.removeItem(item);
        this._size--;
        return item.value;
    }
    forEach(callbackfn, thisArg) {
        const state = this._state;
        let current = this._head;
        while (current) {
            if (thisArg) {
                callbackfn.bind(thisArg)(current.value, current.key, this);
            }
            else {
                callbackfn(current.value, current.key, this);
            }
            if (this._state !== state) {
                throw new Error(`LinkedMap got modified during iteration.`);
            }
            current = current.next;
        }
    }
    keys() {
        const map = this;
        const state = this._state;
        let current = this._head;
        const iterator = {
            [Symbol.iterator]() {
                return iterator;
            },
            next() {
                if (map._state !== state) {
                    throw new Error(`LinkedMap got modified during iteration.`);
                }
                if (current) {
                    const result = { value: current.key, done: false };
                    current = current.next;
                    return result;
                }
                else {
                    return { value: undefined, done: true };
                }
            }
        };
        return iterator;
    }
    values() {
        const map = this;
        const state = this._state;
        let current = this._head;
        const iterator = {
            [Symbol.iterator]() {
                return iterator;
            },
            next() {
                if (map._state !== state) {
                    throw new Error(`LinkedMap got modified during iteration.`);
                }
                if (current) {
                    const result = { value: current.value, done: false };
                    current = current.next;
                    return result;
                }
                else {
                    return { value: undefined, done: true };
                }
            }
        };
        return iterator;
    }
    entries() {
        const map = this;
        const state = this._state;
        let current = this._head;
        const iterator = {
            [Symbol.iterator]() {
                return iterator;
            },
            next() {
                if (map._state !== state) {
                    throw new Error(`LinkedMap got modified during iteration.`);
                }
                if (current) {
                    const result = { value: [current.key, current.value], done: false };
                    current = current.next;
                    return result;
                }
                else {
                    return { value: undefined, done: true };
                }
            }
        };
        return iterator;
    }
    [Symbol.iterator]() {
        return this.entries();
    }
    trimOld(newSize) {
        if (newSize >= this.size) {
            return;
        }
        if (newSize === 0) {
            this.clear();
            return;
        }
        let current = this._head;
        let currentSize = this.size;
        while (current && currentSize > newSize) {
            this._map.delete(current.key);
            current = current.next;
            currentSize--;
        }
        this._head = current;
        this._size = currentSize;
        if (current) {
            current.previous = undefined;
        }
        this._state++;
    }
    addItemFirst(item) {
        // First time Insert
        if (!this._head && !this._tail) {
            this._tail = item;
        }
        else if (!this._head) {
            throw new Error('Invalid list');
        }
        else {
            item.next = this._head;
            this._head.previous = item;
        }
        this._head = item;
        this._state++;
    }
    addItemLast(item) {
        // First time Insert
        if (!this._head && !this._tail) {
            this._head = item;
        }
        else if (!this._tail) {
            throw new Error('Invalid list');
        }
        else {
            item.previous = this._tail;
            this._tail.next = item;
        }
        this._tail = item;
        this._state++;
    }
    removeItem(item) {
        if (item === this._head && item === this._tail) {
            this._head = undefined;
            this._tail = undefined;
        }
        else if (item === this._head) {
            // This can only happend if size === 1 which is handle
            // by the case above.
            if (!item.next) {
                throw new Error('Invalid list');
            }
            item.next.previous = undefined;
            this._head = item.next;
        }
        else if (item === this._tail) {
            // This can only happend if size === 1 which is handle
            // by the case above.
            if (!item.previous) {
                throw new Error('Invalid list');
            }
            item.previous.next = undefined;
            this._tail = item.previous;
        }
        else {
            const next = item.next;
            const previous = item.previous;
            if (!next || !previous) {
                throw new Error('Invalid list');
            }
            next.previous = previous;
            previous.next = next;
        }
        item.next = undefined;
        item.previous = undefined;
        this._state++;
    }
    touch(item, touch) {
        if (!this._head || !this._tail) {
            throw new Error('Invalid list');
        }
        if ((touch !== Touch.First && touch !== Touch.Last)) {
            return;
        }
        if (touch === Touch.First) {
            if (item === this._head) {
                return;
            }
            const next = item.next;
            const previous = item.previous;
            // Unlink the item
            if (item === this._tail) {
                // previous must be defined since item was not head but is tail
                // So there are more than on item in the map
                previous.next = undefined;
                this._tail = previous;
            }
            else {
                // Both next and previous are not undefined since item was neither head nor tail.
                next.previous = previous;
                previous.next = next;
            }
            // Insert the node at head
            item.previous = undefined;
            item.next = this._head;
            this._head.previous = item;
            this._head = item;
            this._state++;
        }
        else if (touch === Touch.Last) {
            if (item === this._tail) {
                return;
            }
            const next = item.next;
            const previous = item.previous;
            // Unlink the item.
            if (item === this._head) {
                // next must be defined since item was not tail but is head
                // So there are more than on item in the map
                next.previous = undefined;
                this._head = next;
            }
            else {
                // Both next and previous are not undefined since item was neither head nor tail.
                next.previous = previous;
                previous.next = next;
            }
            item.next = undefined;
            item.previous = this._tail;
            this._tail.next = item;
            this._tail = item;
            this._state++;
        }
    }
    toJSON() {
        const data = [];
        this.forEach((value, key) => {
            data.push([key, value]);
        });
        return data;
    }
    fromJSON(data) {
        this.clear();
        for (const [key, value] of data) {
            this.set(key, value);
        }
    }
}
exports.LinkedMap = LinkedMap;
class LRUCache extends LinkedMap {
    constructor(limit, ratio = 1) {
        super();
        this._limit = limit;
        this._ratio = Math.min(Math.max(0, ratio), 1);
    }
    get limit() {
        return this._limit;
    }
    set limit(limit) {
        this._limit = limit;
        this.checkTrim();
    }
    get ratio() {
        return this._ratio;
    }
    set ratio(ratio) {
        this._ratio = Math.min(Math.max(0, ratio), 1);
        this.checkTrim();
    }
    get(key, touch = Touch.AsNew) {
        return super.get(key, touch);
    }
    peek(key) {
        return super.get(key, Touch.None);
    }
    set(key, value) {
        super.set(key, value, Touch.Last);
        this.checkTrim();
        return this;
    }
    checkTrim() {
        if (this.size > this._limit) {
            this.trimOld(Math.round(this._limit * this._ratio));
        }
    }
}
exports.LRUCache = LRUCache;
//# sourceMappingURL=linkedMap.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messageBuffer.js":
/*!****************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messageBuffer.js ***!
  \****************************************************************************************/
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.AbstractMessageBuffer = void 0;
const CR = 13;
const LF = 10;
const CRLF = '\r\n';
class AbstractMessageBuffer {
    constructor(encoding = 'utf-8') {
        this._encoding = encoding;
        this._chunks = [];
        this._totalLength = 0;
    }
    get encoding() {
        return this._encoding;
    }
    append(chunk) {
        const toAppend = typeof chunk === 'string' ? this.fromString(chunk, this._encoding) : chunk;
        this._chunks.push(toAppend);
        this._totalLength += toAppend.byteLength;
    }
    tryReadHeaders() {
        if (this._chunks.length === 0) {
            return undefined;
        }
        let state = 0;
        let chunkIndex = 0;
        let offset = 0;
        let chunkBytesRead = 0;
        row: while (chunkIndex < this._chunks.length) {
            const chunk = this._chunks[chunkIndex];
            offset = 0;
            column: while (offset < chunk.length) {
                const value = chunk[offset];
                switch (value) {
                    case CR:
                        switch (state) {
                            case 0:
                                state = 1;
                                break;
                            case 2:
                                state = 3;
                                break;
                            default:
                                state = 0;
                        }
                        break;
                    case LF:
                        switch (state) {
                            case 1:
                                state = 2;
                                break;
                            case 3:
                                state = 4;
                                offset++;
                                break row;
                            default:
                                state = 0;
                        }
                        break;
                    default:
                        state = 0;
                }
                offset++;
            }
            chunkBytesRead += chunk.byteLength;
            chunkIndex++;
        }
        if (state !== 4) {
            return undefined;
        }
        // The buffer contains the two CRLF at the end. So we will
        // have two empty lines after the split at the end as well.
        const buffer = this._read(chunkBytesRead + offset);
        const result = new Map();
        const headers = this.toString(buffer, 'ascii').split(CRLF);
        if (headers.length < 2) {
            return result;
        }
        for (let i = 0; i < headers.length - 2; i++) {
            const header = headers[i];
            const index = header.indexOf(':');
            if (index === -1) {
                throw new Error('Message header must separate key and value using :');
            }
            const key = header.substr(0, index);
            const value = header.substr(index + 1).trim();
            result.set(key, value);
        }
        return result;
    }
    tryReadBody(length) {
        if (this._totalLength < length) {
            return undefined;
        }
        return this._read(length);
    }
    get numberOfBytes() {
        return this._totalLength;
    }
    _read(byteCount) {
        if (byteCount === 0) {
            return this.emptyBuffer();
        }
        if (byteCount > this._totalLength) {
            throw new Error(`Cannot read so many bytes!`);
        }
        if (this._chunks[0].byteLength === byteCount) {
            // super fast path, precisely first chunk must be returned
            const chunk = this._chunks[0];
            this._chunks.shift();
            this._totalLength -= byteCount;
            return this.asNative(chunk);
        }
        if (this._chunks[0].byteLength > byteCount) {
            // fast path, the reading is entirely within the first chunk
            const chunk = this._chunks[0];
            const result = this.asNative(chunk, byteCount);
            this._chunks[0] = chunk.slice(byteCount);
            this._totalLength -= byteCount;
            return result;
        }
        const result = this.allocNative(byteCount);
        let resultOffset = 0;
        let chunkIndex = 0;
        while (byteCount > 0) {
            const chunk = this._chunks[chunkIndex];
            if (chunk.byteLength > byteCount) {
                // this chunk will survive
                const chunkPart = chunk.slice(0, byteCount);
                result.set(chunkPart, resultOffset);
                resultOffset += byteCount;
                this._chunks[chunkIndex] = chunk.slice(byteCount);
                this._totalLength -= byteCount;
                byteCount -= byteCount;
            }
            else {
                // this chunk will be entirely read
                result.set(chunk, resultOffset);
                resultOffset += chunk.byteLength;
                this._chunks.shift();
                this._totalLength -= chunk.byteLength;
                byteCount -= chunk.byteLength;
            }
        }
        return result;
    }
}
exports.AbstractMessageBuffer = AbstractMessageBuffer;
//# sourceMappingURL=messageBuffer.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messageReader.js":
/*!****************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messageReader.js ***!
  \****************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ReadableStreamMessageReader = exports.AbstractMessageReader = exports.MessageReader = void 0;
const ral_1 = __webpack_require__(/*! ./ral */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/ral.js");
const Is = __webpack_require__(/*! ./is */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/is.js");
const events_1 = __webpack_require__(/*! ./events */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/events.js");
var MessageReader;
(function (MessageReader) {
    function is(value) {
        let candidate = value;
        return candidate && Is.func(candidate.listen) && Is.func(candidate.dispose) &&
            Is.func(candidate.onError) && Is.func(candidate.onClose) && Is.func(candidate.onPartialMessage);
    }
    MessageReader.is = is;
})(MessageReader = exports.MessageReader || (exports.MessageReader = {}));
class AbstractMessageReader {
    constructor() {
        this.errorEmitter = new events_1.Emitter();
        this.closeEmitter = new events_1.Emitter();
        this.partialMessageEmitter = new events_1.Emitter();
    }
    dispose() {
        this.errorEmitter.dispose();
        this.closeEmitter.dispose();
    }
    get onError() {
        return this.errorEmitter.event;
    }
    fireError(error) {
        this.errorEmitter.fire(this.asError(error));
    }
    get onClose() {
        return this.closeEmitter.event;
    }
    fireClose() {
        this.closeEmitter.fire(undefined);
    }
    get onPartialMessage() {
        return this.partialMessageEmitter.event;
    }
    firePartialMessage(info) {
        this.partialMessageEmitter.fire(info);
    }
    asError(error) {
        if (error instanceof Error) {
            return error;
        }
        else {
            return new Error(`Reader received error. Reason: ${Is.string(error.message) ? error.message : 'unknown'}`);
        }
    }
}
exports.AbstractMessageReader = AbstractMessageReader;
var ResolvedMessageReaderOptions;
(function (ResolvedMessageReaderOptions) {
    function fromOptions(options) {
        var _a;
        let charset;
        let result;
        let contentDecoder;
        const contentDecoders = new Map();
        let contentTypeDecoder;
        const contentTypeDecoders = new Map();
        if (options === undefined || typeof options === 'string') {
            charset = options !== null && options !== void 0 ? options : 'utf-8';
        }
        else {
            charset = (_a = options.charset) !== null && _a !== void 0 ? _a : 'utf-8';
            if (options.contentDecoder !== undefined) {
                contentDecoder = options.contentDecoder;
                contentDecoders.set(contentDecoder.name, contentDecoder);
            }
            if (options.contentDecoders !== undefined) {
                for (const decoder of options.contentDecoders) {
                    contentDecoders.set(decoder.name, decoder);
                }
            }
            if (options.contentTypeDecoder !== undefined) {
                contentTypeDecoder = options.contentTypeDecoder;
                contentTypeDecoders.set(contentTypeDecoder.name, contentTypeDecoder);
            }
            if (options.contentTypeDecoders !== undefined) {
                for (const decoder of options.contentTypeDecoders) {
                    contentTypeDecoders.set(decoder.name, decoder);
                }
            }
        }
        if (contentTypeDecoder === undefined) {
            contentTypeDecoder = ral_1.default().applicationJson.decoder;
            contentTypeDecoders.set(contentTypeDecoder.name, contentTypeDecoder);
        }
        return { charset, contentDecoder, contentDecoders, contentTypeDecoder, contentTypeDecoders };
    }
    ResolvedMessageReaderOptions.fromOptions = fromOptions;
})(ResolvedMessageReaderOptions || (ResolvedMessageReaderOptions = {}));
class ReadableStreamMessageReader extends AbstractMessageReader {
    constructor(readable, options) {
        super();
        this.readable = readable;
        this.options = ResolvedMessageReaderOptions.fromOptions(options);
        this.buffer = ral_1.default().messageBuffer.create(this.options.charset);
        this._partialMessageTimeout = 10000;
        this.nextMessageLength = -1;
        this.messageToken = 0;
    }
    set partialMessageTimeout(timeout) {
        this._partialMessageTimeout = timeout;
    }
    get partialMessageTimeout() {
        return this._partialMessageTimeout;
    }
    listen(callback) {
        this.nextMessageLength = -1;
        this.messageToken = 0;
        this.partialMessageTimer = undefined;
        this.callback = callback;
        const result = this.readable.onData((data) => {
            this.onData(data);
        });
        this.readable.onError((error) => this.fireError(error));
        this.readable.onClose(() => this.fireClose());
        return result;
    }
    onData(data) {
        this.buffer.append(data);
        while (true) {
            if (this.nextMessageLength === -1) {
                const headers = this.buffer.tryReadHeaders();
                if (!headers) {
                    return;
                }
                const contentLength = headers.get('Content-Length');
                if (!contentLength) {
                    throw new Error('Header must provide a Content-Length property.');
                }
                const length = parseInt(contentLength);
                if (isNaN(length)) {
                    throw new Error('Content-Length value must be a number.');
                }
                this.nextMessageLength = length;
            }
            const body = this.buffer.tryReadBody(this.nextMessageLength);
            if (body === undefined) {
                /** We haven't received the full message yet. */
                this.setPartialMessageTimer();
                return;
            }
            this.clearPartialMessageTimer();
            this.nextMessageLength = -1;
            let p;
            if (this.options.contentDecoder !== undefined) {
                p = this.options.contentDecoder.decode(body);
            }
            else {
                p = Promise.resolve(body);
            }
            p.then((value) => {
                this.options.contentTypeDecoder.decode(value, this.options).then((msg) => {
                    this.callback(msg);
                }, (error) => {
                    this.fireError(error);
                });
            }, (error) => {
                this.fireError(error);
            });
        }
    }
    clearPartialMessageTimer() {
        if (this.partialMessageTimer) {
            ral_1.default().timer.clearTimeout(this.partialMessageTimer);
            this.partialMessageTimer = undefined;
        }
    }
    setPartialMessageTimer() {
        this.clearPartialMessageTimer();
        if (this._partialMessageTimeout <= 0) {
            return;
        }
        this.partialMessageTimer = ral_1.default().timer.setTimeout((token, timeout) => {
            this.partialMessageTimer = undefined;
            if (token === this.messageToken) {
                this.firePartialMessage({ messageToken: token, waitingTime: timeout });
                this.setPartialMessageTimer();
            }
        }, this._partialMessageTimeout, this.messageToken, this._partialMessageTimeout);
    }
}
exports.ReadableStreamMessageReader = ReadableStreamMessageReader;
//# sourceMappingURL=messageReader.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messageWriter.js":
/*!****************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messageWriter.js ***!
  \****************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WriteableStreamMessageWriter = exports.AbstractMessageWriter = exports.MessageWriter = void 0;
const ral_1 = __webpack_require__(/*! ./ral */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/ral.js");
const Is = __webpack_require__(/*! ./is */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/is.js");
const semaphore_1 = __webpack_require__(/*! ./semaphore */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/semaphore.js");
const events_1 = __webpack_require__(/*! ./events */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/events.js");
const ContentLength = 'Content-Length: ';
const CRLF = '\r\n';
var MessageWriter;
(function (MessageWriter) {
    function is(value) {
        let candidate = value;
        return candidate && Is.func(candidate.dispose) && Is.func(candidate.onClose) &&
            Is.func(candidate.onError) && Is.func(candidate.write);
    }
    MessageWriter.is = is;
})(MessageWriter = exports.MessageWriter || (exports.MessageWriter = {}));
class AbstractMessageWriter {
    constructor() {
        this.errorEmitter = new events_1.Emitter();
        this.closeEmitter = new events_1.Emitter();
    }
    dispose() {
        this.errorEmitter.dispose();
        this.closeEmitter.dispose();
    }
    get onError() {
        return this.errorEmitter.event;
    }
    fireError(error, message, count) {
        this.errorEmitter.fire([this.asError(error), message, count]);
    }
    get onClose() {
        return this.closeEmitter.event;
    }
    fireClose() {
        this.closeEmitter.fire(undefined);
    }
    asError(error) {
        if (error instanceof Error) {
            return error;
        }
        else {
            return new Error(`Writer received error. Reason: ${Is.string(error.message) ? error.message : 'unknown'}`);
        }
    }
}
exports.AbstractMessageWriter = AbstractMessageWriter;
var ResolvedMessageWriterOptions;
(function (ResolvedMessageWriterOptions) {
    function fromOptions(options) {
        var _a, _b;
        if (options === undefined || typeof options === 'string') {
            return { charset: options !== null && options !== void 0 ? options : 'utf-8', contentTypeEncoder: ral_1.default().applicationJson.encoder };
        }
        else {
            return { charset: (_a = options.charset) !== null && _a !== void 0 ? _a : 'utf-8', contentEncoder: options.contentEncoder, contentTypeEncoder: (_b = options.contentTypeEncoder) !== null && _b !== void 0 ? _b : ral_1.default().applicationJson.encoder };
        }
    }
    ResolvedMessageWriterOptions.fromOptions = fromOptions;
})(ResolvedMessageWriterOptions || (ResolvedMessageWriterOptions = {}));
class WriteableStreamMessageWriter extends AbstractMessageWriter {
    constructor(writable, options) {
        super();
        this.writable = writable;
        this.options = ResolvedMessageWriterOptions.fromOptions(options);
        this.errorCount = 0;
        this.writeSemaphore = new semaphore_1.Semaphore(1);
        this.writable.onError((error) => this.fireError(error));
        this.writable.onClose(() => this.fireClose());
    }
    async write(msg) {
        return this.writeSemaphore.lock(async () => {
            const payload = this.options.contentTypeEncoder.encode(msg, this.options).then((buffer) => {
                if (this.options.contentEncoder !== undefined) {
                    return this.options.contentEncoder.encode(buffer);
                }
                else {
                    return buffer;
                }
            });
            return payload.then((buffer) => {
                const headers = [];
                headers.push(ContentLength, buffer.byteLength.toString(), CRLF);
                headers.push(CRLF);
                return this.doWrite(msg, headers, buffer);
            }, (error) => {
                this.fireError(error);
                throw error;
            });
        });
    }
    async doWrite(msg, headers, data) {
        try {
            await this.writable.write(headers.join(''), 'ascii');
            return this.writable.write(data);
        }
        catch (error) {
            this.handleError(error, msg);
            return Promise.reject(error);
        }
    }
    handleError(error, msg) {
        this.errorCount++;
        this.fireError(error, msg, this.errorCount);
    }
    end() {
        this.writable.end();
    }
}
exports.WriteableStreamMessageWriter = WriteableStreamMessageWriter;
//# sourceMappingURL=messageWriter.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messages.js":
/*!***********************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/messages.js ***!
  \***********************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.isResponseMessage = exports.isNotificationMessage = exports.isRequestMessage = exports.NotificationType9 = exports.NotificationType8 = exports.NotificationType7 = exports.NotificationType6 = exports.NotificationType5 = exports.NotificationType4 = exports.NotificationType3 = exports.NotificationType2 = exports.NotificationType1 = exports.NotificationType0 = exports.NotificationType = exports.RequestType9 = exports.RequestType8 = exports.RequestType7 = exports.RequestType6 = exports.RequestType5 = exports.RequestType4 = exports.RequestType3 = exports.RequestType2 = exports.RequestType1 = exports.RequestType = exports.RequestType0 = exports.AbstractMessageSignature = exports.ParameterStructures = exports.ResponseError = exports.ErrorCodes = void 0;
const is = __webpack_require__(/*! ./is */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/is.js");
/**
 * Predefined error codes.
 */
var ErrorCodes;
(function (ErrorCodes) {
    // Defined by JSON RPC
    ErrorCodes.ParseError = -32700;
    ErrorCodes.InvalidRequest = -32600;
    ErrorCodes.MethodNotFound = -32601;
    ErrorCodes.InvalidParams = -32602;
    ErrorCodes.InternalError = -32603;
    /**
     * This is the start range of JSON RPC reserved error codes.
     * It doesn't denote a real error code. No application error codes should
     * be defined between the start and end range. For backwards
     * compatibility the `ServerNotInitialized` and the `UnknownErrorCode`
     * are left in the range.
     *
     * @since 3.16.0
    */
    ErrorCodes.jsonrpcReservedErrorRangeStart = -32099;
    /** @deprecated use  jsonrpcReservedErrorRangeStart */
    ErrorCodes.serverErrorStart = ErrorCodes.jsonrpcReservedErrorRangeStart;
    ErrorCodes.MessageWriteError = -32099;
    ErrorCodes.MessageReadError = -32098;
    ErrorCodes.ServerNotInitialized = -32002;
    ErrorCodes.UnknownErrorCode = -32001;
    /**
     * This is the end range of JSON RPC reserved error codes.
     * It doesn't denote a real error code.
     *
     * @since 3.16.0
    */
    ErrorCodes.jsonrpcReservedErrorRangeEnd = -32000;
    /** @deprecated use  jsonrpcReservedErrorRangeEnd */
    ErrorCodes.serverErrorEnd = ErrorCodes.jsonrpcReservedErrorRangeEnd;
})(ErrorCodes = exports.ErrorCodes || (exports.ErrorCodes = {}));
/**
 * An error object return in a response in case a request
 * has failed.
 */
class ResponseError extends Error {
    constructor(code, message, data) {
        super(message);
        this.code = is.number(code) ? code : ErrorCodes.UnknownErrorCode;
        this.data = data;
        Object.setPrototypeOf(this, ResponseError.prototype);
    }
    toJson() {
        return {
            code: this.code,
            message: this.message,
            data: this.data,
        };
    }
}
exports.ResponseError = ResponseError;
class ParameterStructures {
    constructor(kind) {
        this.kind = kind;
    }
    static is(value) {
        return value === ParameterStructures.auto || value === ParameterStructures.byName || value === ParameterStructures.byPosition;
    }
    toString() {
        return this.kind;
    }
}
exports.ParameterStructures = ParameterStructures;
/**
 * The parameter structure is automatically inferred on the number of parameters
 * and the parameter type in case of a single param.
 */
ParameterStructures.auto = new ParameterStructures('auto');
/**
 * Forces `byPosition` parameter structure. This is useful if you have a single
 * parameter which has a literal type.
 */
ParameterStructures.byPosition = new ParameterStructures('byPosition');
/**
 * Forces `byName` parameter structure. This is only useful when having a single
 * parameter. The library will report errors if used with a different number of
 * parameters.
 */
ParameterStructures.byName = new ParameterStructures('byName');
/**
 * An abstract implementation of a MessageType.
 */
class AbstractMessageSignature {
    constructor(method, numberOfParams) {
        this.method = method;
        this.numberOfParams = numberOfParams;
    }
    get parameterStructures() {
        return ParameterStructures.auto;
    }
}
exports.AbstractMessageSignature = AbstractMessageSignature;
/**
 * Classes to type request response pairs
 */
class RequestType0 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 0);
    }
}
exports.RequestType0 = RequestType0;
class RequestType extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
        super(method, 1);
        this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
        return this._parameterStructures;
    }
}
exports.RequestType = RequestType;
class RequestType1 extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
        super(method, 1);
        this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
        return this._parameterStructures;
    }
}
exports.RequestType1 = RequestType1;
class RequestType2 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 2);
    }
}
exports.RequestType2 = RequestType2;
class RequestType3 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 3);
    }
}
exports.RequestType3 = RequestType3;
class RequestType4 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 4);
    }
}
exports.RequestType4 = RequestType4;
class RequestType5 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 5);
    }
}
exports.RequestType5 = RequestType5;
class RequestType6 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 6);
    }
}
exports.RequestType6 = RequestType6;
class RequestType7 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 7);
    }
}
exports.RequestType7 = RequestType7;
class RequestType8 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 8);
    }
}
exports.RequestType8 = RequestType8;
class RequestType9 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 9);
    }
}
exports.RequestType9 = RequestType9;
class NotificationType extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
        super(method, 1);
        this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
        return this._parameterStructures;
    }
}
exports.NotificationType = NotificationType;
class NotificationType0 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 0);
    }
}
exports.NotificationType0 = NotificationType0;
class NotificationType1 extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
        super(method, 1);
        this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
        return this._parameterStructures;
    }
}
exports.NotificationType1 = NotificationType1;
class NotificationType2 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 2);
    }
}
exports.NotificationType2 = NotificationType2;
class NotificationType3 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 3);
    }
}
exports.NotificationType3 = NotificationType3;
class NotificationType4 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 4);
    }
}
exports.NotificationType4 = NotificationType4;
class NotificationType5 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 5);
    }
}
exports.NotificationType5 = NotificationType5;
class NotificationType6 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 6);
    }
}
exports.NotificationType6 = NotificationType6;
class NotificationType7 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 7);
    }
}
exports.NotificationType7 = NotificationType7;
class NotificationType8 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 8);
    }
}
exports.NotificationType8 = NotificationType8;
class NotificationType9 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 9);
    }
}
exports.NotificationType9 = NotificationType9;
/**
 * Tests if the given message is a request message
 */
function isRequestMessage(message) {
    const candidate = message;
    return candidate && is.string(candidate.method) && (is.string(candidate.id) || is.number(candidate.id));
}
exports.isRequestMessage = isRequestMessage;
/**
 * Tests if the given message is a notification message
 */
function isNotificationMessage(message) {
    const candidate = message;
    return candidate && is.string(candidate.method) && message.id === void 0;
}
exports.isNotificationMessage = isNotificationMessage;
/**
 * Tests if the given message is a response message
 */
function isResponseMessage(message) {
    const candidate = message;
    return candidate && (candidate.result !== void 0 || !!candidate.error) && (is.string(candidate.id) || is.number(candidate.id) || candidate.id === null);
}
exports.isResponseMessage = isResponseMessage;
//# sourceMappingURL=messages.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/ral.js":
/*!******************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/ral.js ***!
  \******************************************************************************/
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
let _ral;
function RAL() {
    if (_ral === undefined) {
        throw new Error(`No runtime abstraction layer installed`);
    }
    return _ral;
}
(function (RAL) {
    function install(ral) {
        if (ral === undefined) {
            throw new Error(`No runtime abstraction layer provided`);
        }
        _ral = ral;
    }
    RAL.install = install;
})(RAL || (RAL = {}));
exports.default = RAL;
//# sourceMappingURL=ral.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/semaphore.js":
/*!************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/semaphore.js ***!
  \************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Semaphore = void 0;
const ral_1 = __webpack_require__(/*! ./ral */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/common/ral.js");
class Semaphore {
    constructor(capacity = 1) {
        if (capacity <= 0) {
            throw new Error('Capacity must be greater than 0');
        }
        this._capacity = capacity;
        this._active = 0;
        this._waiting = [];
    }
    lock(thunk) {
        return new Promise((resolve, reject) => {
            this._waiting.push({ thunk, resolve, reject });
            this.runNext();
        });
    }
    get active() {
        return this._active;
    }
    runNext() {
        if (this._waiting.length === 0 || this._active === this._capacity) {
            return;
        }
        ral_1.default().timer.setImmediate(() => this.doRunNext());
    }
    doRunNext() {
        if (this._waiting.length === 0 || this._active === this._capacity) {
            return;
        }
        const next = this._waiting.shift();
        this._active++;
        if (this._active > this._capacity) {
            throw new Error(`To many thunks active`);
        }
        try {
            const result = next.thunk();
            if (result instanceof Promise) {
                result.then((value) => {
                    this._active--;
                    next.resolve(value);
                    this.runNext();
                }, (err) => {
                    this._active--;
                    next.reject(err);
                    this.runNext();
                });
            }
            else {
                this._active--;
                next.resolve(result);
                this.runNext();
            }
        }
        catch (err) {
            this._active--;
            next.reject(err);
            this.runNext();
        }
    }
}
exports.Semaphore = Semaphore;
//# sourceMappingURL=semaphore.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/browser/main.js":
/*!***************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/browser/main.js ***!
  \***************************************************************************************/
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.LanguageClient = void 0;
const api_1 = __webpack_require__(/*! ../common/api */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/api.js");
const browser_1 = __webpack_require__(/*! vscode-languageserver-protocol/browser */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/browser.js");
__exportStar(__webpack_require__(/*! vscode-languageserver-protocol/browser */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/browser.js"), exports);
__exportStar(__webpack_require__(/*! ../common/api */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/api.js"), exports);
class LanguageClient extends api_1.CommonLanguageClient {
    constructor(id, name, clientOptions, worker) {
        super(id, name, clientOptions);
        this.worker = worker;
    }
    createMessageTransports(_encoding) {
        const reader = new browser_1.BrowserMessageReader(this.worker);
        const writer = new browser_1.BrowserMessageWriter(this.worker);
        return Promise.resolve({ reader, writer });
    }
    getLocale() {
        // ToDo: need to find a way to let the locale
        // travel to the worker extension host.
        return 'en';
    }
}
exports.LanguageClient = LanguageClient;
//# sourceMappingURL=main.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/api.js":
/*!*************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/api.js ***!
  \*************************************************************************************/
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
__exportStar(__webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js"), exports);
__exportStar(__webpack_require__(/*! ./client */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js"), exports);
__exportStar(__webpack_require__(/*! ./commonClient */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/commonClient.js"), exports);
//# sourceMappingURL=api.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/callHierarchy.js":
/*!***********************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/callHierarchy.js ***!
  \***********************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.CallHierarchyFeature = void 0;
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const client_1 = __webpack_require__(/*! ./client */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js");
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class CallHierarchyProvider {
    constructor(client) {
        this.client = client;
        this.middleware = client.clientOptions.middleware;
    }
    prepareCallHierarchy(document, position, token) {
        const client = this.client;
        const middleware = this.middleware;
        const prepareCallHierarchy = (document, position, token) => {
            const params = client.code2ProtocolConverter.asTextDocumentPositionParams(document, position);
            return client.sendRequest(vscode_languageserver_protocol_1.CallHierarchyPrepareRequest.type, params, token).then((result) => {
                return client.protocol2CodeConverter.asCallHierarchyItems(result);
            }, (error) => {
                return client.handleFailedRequest(vscode_languageserver_protocol_1.CallHierarchyPrepareRequest.type, error, null);
            });
        };
        return middleware.prepareCallHierarchy
            ? middleware.prepareCallHierarchy(document, position, token, prepareCallHierarchy)
            : prepareCallHierarchy(document, position, token);
    }
    provideCallHierarchyIncomingCalls(item, token) {
        const client = this.client;
        const middleware = this.middleware;
        const provideCallHierarchyIncomingCalls = (item, token) => {
            const params = {
                item: client.code2ProtocolConverter.asCallHierarchyItem(item)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.CallHierarchyIncomingCallsRequest.type, params, token).then((result) => {
                return client.protocol2CodeConverter.asCallHierarchyIncomingCalls(result);
            }, (error) => {
                return client.handleFailedRequest(vscode_languageserver_protocol_1.CallHierarchyIncomingCallsRequest.type, error, null);
            });
        };
        return middleware.provideCallHierarchyIncomingCalls
            ? middleware.provideCallHierarchyIncomingCalls(item, token, provideCallHierarchyIncomingCalls)
            : provideCallHierarchyIncomingCalls(item, token);
    }
    provideCallHierarchyOutgoingCalls(item, token) {
        const client = this.client;
        const middleware = this.middleware;
        const provideCallHierarchyOutgoingCalls = (item, token) => {
            const params = {
                item: client.code2ProtocolConverter.asCallHierarchyItem(item)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.CallHierarchyOutgoingCallsRequest.type, params, token).then((result) => {
                return client.protocol2CodeConverter.asCallHierarchyOutgoingCalls(result);
            }, (error) => {
                return client.handleFailedRequest(vscode_languageserver_protocol_1.CallHierarchyOutgoingCallsRequest.type, error, null);
            });
        };
        return middleware.provideCallHierarchyOutgoingCalls
            ? middleware.provideCallHierarchyOutgoingCalls(item, token, provideCallHierarchyOutgoingCalls)
            : provideCallHierarchyOutgoingCalls(item, token);
    }
}
class CallHierarchyFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.CallHierarchyPrepareRequest.type);
    }
    fillClientCapabilities(cap) {
        const capabilities = cap;
        const capability = ensure(ensure(capabilities, 'textDocument'), 'callHierarchy');
        capability.dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const [id, options] = this.getRegistration(documentSelector, capabilities.callHierarchyProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const client = this._client;
        const provider = new CallHierarchyProvider(client);
        return [vscode_1.languages.registerCallHierarchyProvider(options.documentSelector, provider), provider];
    }
}
exports.CallHierarchyFeature = CallHierarchyFeature;
//# sourceMappingURL=callHierarchy.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js":
/*!****************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js ***!
  \****************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.BaseLanguageClient = exports.MessageTransports = exports.TextDocumentFeature = exports.State = exports.RevealOutputChannelOn = exports.CloseAction = exports.ErrorAction = void 0;
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const configuration_1 = __webpack_require__(/*! ./configuration */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/configuration.js");
const c2p = __webpack_require__(/*! ./codeConverter */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/codeConverter.js");
const p2c = __webpack_require__(/*! ./protocolConverter */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolConverter.js");
const Is = __webpack_require__(/*! ./utils/is */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/is.js");
const async_1 = __webpack_require__(/*! ./utils/async */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/async.js");
const UUID = __webpack_require__(/*! ./utils/uuid */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/uuid.js");
const progressPart_1 = __webpack_require__(/*! ./progressPart */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/progressPart.js");
class ConsoleLogger {
    error(message) {
        vscode_languageserver_protocol_1.RAL().console.error(message);
    }
    warn(message) {
        vscode_languageserver_protocol_1.RAL().console.warn(message);
    }
    info(message) {
        vscode_languageserver_protocol_1.RAL().console.info(message);
    }
    log(message) {
        vscode_languageserver_protocol_1.RAL().console.log(message);
    }
}
function createConnection(input, output, errorHandler, closeHandler, options) {
    let logger = new ConsoleLogger();
    let connection = vscode_languageserver_protocol_1.createProtocolConnection(input, output, logger, options);
    connection.onError((data) => { errorHandler(data[0], data[1], data[2]); });
    connection.onClose(closeHandler);
    let result = {
        listen: () => connection.listen(),
        sendRequest: (type, ...params) => connection.sendRequest(Is.string(type) ? type : type.method, ...params),
        onRequest: (type, handler) => connection.onRequest(Is.string(type) ? type : type.method, handler),
        sendNotification: (type, params) => connection.sendNotification(Is.string(type) ? type : type.method, params),
        onNotification: (type, handler) => connection.onNotification(Is.string(type) ? type : type.method, handler),
        onProgress: connection.onProgress,
        sendProgress: connection.sendProgress,
        trace: (value, tracer, sendNotificationOrTraceOptions) => {
            const defaultTraceOptions = {
                sendNotification: false,
                traceFormat: vscode_languageserver_protocol_1.TraceFormat.Text
            };
            if (sendNotificationOrTraceOptions === undefined) {
                connection.trace(value, tracer, defaultTraceOptions);
            }
            else if (Is.boolean(sendNotificationOrTraceOptions)) {
                connection.trace(value, tracer, sendNotificationOrTraceOptions);
            }
            else {
                connection.trace(value, tracer, sendNotificationOrTraceOptions);
            }
        },
        initialize: (params) => connection.sendRequest(vscode_languageserver_protocol_1.InitializeRequest.type, params),
        shutdown: () => connection.sendRequest(vscode_languageserver_protocol_1.ShutdownRequest.type, undefined),
        exit: () => connection.sendNotification(vscode_languageserver_protocol_1.ExitNotification.type),
        onLogMessage: (handler) => connection.onNotification(vscode_languageserver_protocol_1.LogMessageNotification.type, handler),
        onShowMessage: (handler) => connection.onNotification(vscode_languageserver_protocol_1.ShowMessageNotification.type, handler),
        onTelemetry: (handler) => connection.onNotification(vscode_languageserver_protocol_1.TelemetryEventNotification.type, handler),
        didChangeConfiguration: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type, params),
        didChangeWatchedFiles: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidChangeWatchedFilesNotification.type, params),
        didOpenTextDocument: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidOpenTextDocumentNotification.type, params),
        didChangeTextDocument: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, params),
        didCloseTextDocument: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidCloseTextDocumentNotification.type, params),
        didSaveTextDocument: (params) => connection.sendNotification(vscode_languageserver_protocol_1.DidSaveTextDocumentNotification.type, params),
        onDiagnostics: (handler) => connection.onNotification(vscode_languageserver_protocol_1.PublishDiagnosticsNotification.type, handler),
        end: () => connection.end(),
        dispose: () => connection.dispose()
    };
    return result;
}
/**
 * An action to be performed when the connection is producing errors.
 */
var ErrorAction;
(function (ErrorAction) {
    /**
     * Continue running the server.
     */
    ErrorAction[ErrorAction["Continue"] = 1] = "Continue";
    /**
     * Shutdown the server.
     */
    ErrorAction[ErrorAction["Shutdown"] = 2] = "Shutdown";
})(ErrorAction = exports.ErrorAction || (exports.ErrorAction = {}));
/**
 * An action to be performed when the connection to a server got closed.
 */
var CloseAction;
(function (CloseAction) {
    /**
     * Don't restart the server. The connection stays closed.
     */
    CloseAction[CloseAction["DoNotRestart"] = 1] = "DoNotRestart";
    /**
     * Restart the server.
     */
    CloseAction[CloseAction["Restart"] = 2] = "Restart";
})(CloseAction = exports.CloseAction || (exports.CloseAction = {}));
class DefaultErrorHandler {
    constructor(name, maxRestartCount) {
        this.name = name;
        this.maxRestartCount = maxRestartCount;
        this.restarts = [];
    }
    error(_error, _message, count) {
        if (count && count <= 3) {
            return ErrorAction.Continue;
        }
        return ErrorAction.Shutdown;
    }
    closed() {
        this.restarts.push(Date.now());
        if (this.restarts.length <= this.maxRestartCount) {
            return CloseAction.Restart;
        }
        else {
            let diff = this.restarts[this.restarts.length - 1] - this.restarts[0];
            if (diff <= 3 * 60 * 1000) {
                vscode_1.window.showErrorMessage(`The ${this.name} server crashed ${this.maxRestartCount + 1} times in the last 3 minutes. The server will not be restarted.`);
                return CloseAction.DoNotRestart;
            }
            else {
                this.restarts.shift();
                return CloseAction.Restart;
            }
        }
    }
}
var RevealOutputChannelOn;
(function (RevealOutputChannelOn) {
    RevealOutputChannelOn[RevealOutputChannelOn["Info"] = 1] = "Info";
    RevealOutputChannelOn[RevealOutputChannelOn["Warn"] = 2] = "Warn";
    RevealOutputChannelOn[RevealOutputChannelOn["Error"] = 3] = "Error";
    RevealOutputChannelOn[RevealOutputChannelOn["Never"] = 4] = "Never";
})(RevealOutputChannelOn = exports.RevealOutputChannelOn || (exports.RevealOutputChannelOn = {}));
var State;
(function (State) {
    State[State["Stopped"] = 1] = "Stopped";
    State[State["Starting"] = 3] = "Starting";
    State[State["Running"] = 2] = "Running";
})(State = exports.State || (exports.State = {}));
var ClientState;
(function (ClientState) {
    ClientState[ClientState["Initial"] = 0] = "Initial";
    ClientState[ClientState["Starting"] = 1] = "Starting";
    ClientState[ClientState["StartFailed"] = 2] = "StartFailed";
    ClientState[ClientState["Running"] = 3] = "Running";
    ClientState[ClientState["Stopping"] = 4] = "Stopping";
    ClientState[ClientState["Stopped"] = 5] = "Stopped";
})(ClientState || (ClientState = {}));
const SupportedSymbolKinds = [
    vscode_languageserver_protocol_1.SymbolKind.File,
    vscode_languageserver_protocol_1.SymbolKind.Module,
    vscode_languageserver_protocol_1.SymbolKind.Namespace,
    vscode_languageserver_protocol_1.SymbolKind.Package,
    vscode_languageserver_protocol_1.SymbolKind.Class,
    vscode_languageserver_protocol_1.SymbolKind.Method,
    vscode_languageserver_protocol_1.SymbolKind.Property,
    vscode_languageserver_protocol_1.SymbolKind.Field,
    vscode_languageserver_protocol_1.SymbolKind.Constructor,
    vscode_languageserver_protocol_1.SymbolKind.Enum,
    vscode_languageserver_protocol_1.SymbolKind.Interface,
    vscode_languageserver_protocol_1.SymbolKind.Function,
    vscode_languageserver_protocol_1.SymbolKind.Variable,
    vscode_languageserver_protocol_1.SymbolKind.Constant,
    vscode_languageserver_protocol_1.SymbolKind.String,
    vscode_languageserver_protocol_1.SymbolKind.Number,
    vscode_languageserver_protocol_1.SymbolKind.Boolean,
    vscode_languageserver_protocol_1.SymbolKind.Array,
    vscode_languageserver_protocol_1.SymbolKind.Object,
    vscode_languageserver_protocol_1.SymbolKind.Key,
    vscode_languageserver_protocol_1.SymbolKind.Null,
    vscode_languageserver_protocol_1.SymbolKind.EnumMember,
    vscode_languageserver_protocol_1.SymbolKind.Struct,
    vscode_languageserver_protocol_1.SymbolKind.Event,
    vscode_languageserver_protocol_1.SymbolKind.Operator,
    vscode_languageserver_protocol_1.SymbolKind.TypeParameter
];
const SupportedCompletionItemKinds = [
    vscode_languageserver_protocol_1.CompletionItemKind.Text,
    vscode_languageserver_protocol_1.CompletionItemKind.Method,
    vscode_languageserver_protocol_1.CompletionItemKind.Function,
    vscode_languageserver_protocol_1.CompletionItemKind.Constructor,
    vscode_languageserver_protocol_1.CompletionItemKind.Field,
    vscode_languageserver_protocol_1.CompletionItemKind.Variable,
    vscode_languageserver_protocol_1.CompletionItemKind.Class,
    vscode_languageserver_protocol_1.CompletionItemKind.Interface,
    vscode_languageserver_protocol_1.CompletionItemKind.Module,
    vscode_languageserver_protocol_1.CompletionItemKind.Property,
    vscode_languageserver_protocol_1.CompletionItemKind.Unit,
    vscode_languageserver_protocol_1.CompletionItemKind.Value,
    vscode_languageserver_protocol_1.CompletionItemKind.Enum,
    vscode_languageserver_protocol_1.CompletionItemKind.Keyword,
    vscode_languageserver_protocol_1.CompletionItemKind.Snippet,
    vscode_languageserver_protocol_1.CompletionItemKind.Color,
    vscode_languageserver_protocol_1.CompletionItemKind.File,
    vscode_languageserver_protocol_1.CompletionItemKind.Reference,
    vscode_languageserver_protocol_1.CompletionItemKind.Folder,
    vscode_languageserver_protocol_1.CompletionItemKind.EnumMember,
    vscode_languageserver_protocol_1.CompletionItemKind.Constant,
    vscode_languageserver_protocol_1.CompletionItemKind.Struct,
    vscode_languageserver_protocol_1.CompletionItemKind.Event,
    vscode_languageserver_protocol_1.CompletionItemKind.Operator,
    vscode_languageserver_protocol_1.CompletionItemKind.TypeParameter
];
const SupportedSymbolTags = [
    vscode_languageserver_protocol_1.SymbolTag.Deprecated
];
function ensure(target, key) {
    if (target[key] === undefined) {
        target[key] = {};
    }
    return target[key];
}
var FileFormattingOptions;
(function (FileFormattingOptions) {
    function fromConfiguration(document) {
        const filesConfig = vscode_1.workspace.getConfiguration('files', document);
        return {
            trimTrailingWhitespace: filesConfig.get('trimTrailingWhitespace'),
            trimFinalNewlines: filesConfig.get('trimFinalNewlines'),
            insertFinalNewline: filesConfig.get('insertFinalNewline'),
        };
    }
    FileFormattingOptions.fromConfiguration = fromConfiguration;
})(FileFormattingOptions || (FileFormattingOptions = {}));
var DynamicFeature;
(function (DynamicFeature) {
    function is(value) {
        let candidate = value;
        return candidate && Is.func(candidate.register) && Is.func(candidate.unregister) && Is.func(candidate.dispose) && candidate.registrationType !== undefined;
    }
    DynamicFeature.is = is;
})(DynamicFeature || (DynamicFeature = {}));
class DocumentNotifications {
    constructor(_client, _event, _type, _middleware, _createParams, _selectorFilter) {
        this._client = _client;
        this._event = _event;
        this._type = _type;
        this._middleware = _middleware;
        this._createParams = _createParams;
        this._selectorFilter = _selectorFilter;
        this._selectors = new Map();
    }
    static textDocumentFilter(selectors, textDocument) {
        for (const selector of selectors) {
            if (vscode_1.languages.match(selector, textDocument)) {
                return true;
            }
        }
        return false;
    }
    register(data) {
        if (!data.registerOptions.documentSelector) {
            return;
        }
        if (!this._listener) {
            this._listener = this._event(this.callback, this);
        }
        this._selectors.set(data.id, data.registerOptions.documentSelector);
    }
    callback(data) {
        if (!this._selectorFilter || this._selectorFilter(this._selectors.values(), data)) {
            if (this._middleware) {
                this._middleware(data, (data) => this._client.sendNotification(this._type, this._createParams(data)));
            }
            else {
                this._client.sendNotification(this._type, this._createParams(data));
            }
            this.notificationSent(data);
        }
    }
    notificationSent(_data) {
    }
    unregister(id) {
        this._selectors.delete(id);
        if (this._selectors.size === 0 && this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    dispose() {
        this._selectors.clear();
        if (this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    getProvider(document) {
        for (const selector of this._selectors.values()) {
            if (vscode_1.languages.match(selector, document)) {
                return {
                    send: (data) => {
                        this.callback(data);
                    }
                };
            }
        }
        return undefined;
    }
}
class DidOpenTextDocumentFeature extends DocumentNotifications {
    constructor(client, _syncedDocuments) {
        super(client, vscode_1.workspace.onDidOpenTextDocument, vscode_languageserver_protocol_1.DidOpenTextDocumentNotification.type, client.clientOptions.middleware.didOpen, (textDocument) => client.code2ProtocolConverter.asOpenTextDocumentParams(textDocument), DocumentNotifications.textDocumentFilter);
        this._syncedDocuments = _syncedDocuments;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'synchronization').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.openClose) {
            this.register({ id: UUID.generateUuid(), registerOptions: { documentSelector: documentSelector } });
        }
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidOpenTextDocumentNotification.type;
    }
    register(data) {
        super.register(data);
        if (!data.registerOptions.documentSelector) {
            return;
        }
        let documentSelector = data.registerOptions.documentSelector;
        vscode_1.workspace.textDocuments.forEach((textDocument) => {
            let uri = textDocument.uri.toString();
            if (this._syncedDocuments.has(uri)) {
                return;
            }
            if (vscode_1.languages.match(documentSelector, textDocument)) {
                let middleware = this._client.clientOptions.middleware;
                let didOpen = (textDocument) => {
                    this._client.sendNotification(this._type, this._createParams(textDocument));
                };
                if (middleware.didOpen) {
                    middleware.didOpen(textDocument, didOpen);
                }
                else {
                    didOpen(textDocument);
                }
                this._syncedDocuments.set(uri, textDocument);
            }
        });
    }
    notificationSent(textDocument) {
        super.notificationSent(textDocument);
        this._syncedDocuments.set(textDocument.uri.toString(), textDocument);
    }
}
class DidCloseTextDocumentFeature extends DocumentNotifications {
    constructor(client, _syncedDocuments) {
        super(client, vscode_1.workspace.onDidCloseTextDocument, vscode_languageserver_protocol_1.DidCloseTextDocumentNotification.type, client.clientOptions.middleware.didClose, (textDocument) => client.code2ProtocolConverter.asCloseTextDocumentParams(textDocument), DocumentNotifications.textDocumentFilter);
        this._syncedDocuments = _syncedDocuments;
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidCloseTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'synchronization').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.openClose) {
            this.register({ id: UUID.generateUuid(), registerOptions: { documentSelector: documentSelector } });
        }
    }
    notificationSent(textDocument) {
        super.notificationSent(textDocument);
        this._syncedDocuments.delete(textDocument.uri.toString());
    }
    unregister(id) {
        let selector = this._selectors.get(id);
        // The super call removed the selector from the map
        // of selectors.
        super.unregister(id);
        let selectors = this._selectors.values();
        this._syncedDocuments.forEach((textDocument) => {
            if (vscode_1.languages.match(selector, textDocument) && !this._selectorFilter(selectors, textDocument)) {
                let middleware = this._client.clientOptions.middleware;
                let didClose = (textDocument) => {
                    this._client.sendNotification(this._type, this._createParams(textDocument));
                };
                this._syncedDocuments.delete(textDocument.uri.toString());
                if (middleware.didClose) {
                    middleware.didClose(textDocument, didClose);
                }
                else {
                    didClose(textDocument);
                }
            }
        });
    }
}
class DidChangeTextDocumentFeature {
    constructor(_client) {
        this._client = _client;
        this._changeData = new Map();
        this._forcingDelivery = false;
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'synchronization').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.change !== undefined && textDocumentSyncOptions.change !== vscode_languageserver_protocol_1.TextDocumentSyncKind.None) {
            this.register({
                id: UUID.generateUuid(),
                registerOptions: Object.assign({}, { documentSelector: documentSelector }, { syncKind: textDocumentSyncOptions.change })
            });
        }
    }
    register(data) {
        if (!data.registerOptions.documentSelector) {
            return;
        }
        if (!this._listener) {
            this._listener = vscode_1.workspace.onDidChangeTextDocument(this.callback, this);
        }
        this._changeData.set(data.id, {
            documentSelector: data.registerOptions.documentSelector,
            syncKind: data.registerOptions.syncKind
        });
    }
    callback(event) {
        // Text document changes are send for dirty changes as well. We don't
        // have dirty / un-dirty events in the LSP so we ignore content changes
        // with length zero.
        if (event.contentChanges.length === 0) {
            return;
        }
        for (const changeData of this._changeData.values()) {
            if (vscode_1.languages.match(changeData.documentSelector, event.document)) {
                let middleware = this._client.clientOptions.middleware;
                if (changeData.syncKind === vscode_languageserver_protocol_1.TextDocumentSyncKind.Incremental) {
                    let params = this._client.code2ProtocolConverter.asChangeTextDocumentParams(event);
                    if (middleware.didChange) {
                        middleware.didChange(event, () => this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, params));
                    }
                    else {
                        this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, params);
                    }
                }
                else if (changeData.syncKind === vscode_languageserver_protocol_1.TextDocumentSyncKind.Full) {
                    let didChange = (event) => {
                        if (this._changeDelayer) {
                            if (this._changeDelayer.uri !== event.document.uri.toString()) {
                                // Use this force delivery to track boolean state. Otherwise we might call two times.
                                this.forceDelivery();
                                this._changeDelayer.uri = event.document.uri.toString();
                            }
                            this._changeDelayer.delayer.trigger(() => {
                                this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, this._client.code2ProtocolConverter.asChangeTextDocumentParams(event.document));
                            });
                        }
                        else {
                            this._changeDelayer = {
                                uri: event.document.uri.toString(),
                                delayer: new async_1.Delayer(200)
                            };
                            this._changeDelayer.delayer.trigger(() => {
                                this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, this._client.code2ProtocolConverter.asChangeTextDocumentParams(event.document));
                            }, -1);
                        }
                    };
                    if (middleware.didChange) {
                        middleware.didChange(event, didChange);
                    }
                    else {
                        didChange(event);
                    }
                }
            }
        }
    }
    unregister(id) {
        this._changeData.delete(id);
        if (this._changeData.size === 0 && this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    dispose() {
        this._changeDelayer = undefined;
        this._forcingDelivery = false;
        this._changeData.clear();
        if (this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    forceDelivery() {
        if (this._forcingDelivery || !this._changeDelayer) {
            return;
        }
        try {
            this._forcingDelivery = true;
            this._changeDelayer.delayer.forceDelivery();
        }
        finally {
            this._forcingDelivery = false;
        }
    }
    getProvider(document) {
        for (const changeData of this._changeData.values()) {
            if (vscode_1.languages.match(changeData.documentSelector, document)) {
                return {
                    send: (event) => {
                        this.callback(event);
                    }
                };
            }
        }
        return undefined;
    }
}
class WillSaveFeature extends DocumentNotifications {
    constructor(client) {
        super(client, vscode_1.workspace.onWillSaveTextDocument, vscode_languageserver_protocol_1.WillSaveTextDocumentNotification.type, client.clientOptions.middleware.willSave, (willSaveEvent) => client.code2ProtocolConverter.asWillSaveTextDocumentParams(willSaveEvent), (selectors, willSaveEvent) => DocumentNotifications.textDocumentFilter(selectors, willSaveEvent.document));
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.WillSaveTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        let value = ensure(ensure(capabilities, 'textDocument'), 'synchronization');
        value.willSave = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.willSave) {
            this.register({
                id: UUID.generateUuid(),
                registerOptions: { documentSelector: documentSelector }
            });
        }
    }
}
class WillSaveWaitUntilFeature {
    constructor(_client) {
        this._client = _client;
        this._selectors = new Map();
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.WillSaveTextDocumentWaitUntilRequest.type;
    }
    fillClientCapabilities(capabilities) {
        let value = ensure(ensure(capabilities, 'textDocument'), 'synchronization');
        value.willSaveWaitUntil = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.willSaveWaitUntil) {
            this.register({
                id: UUID.generateUuid(),
                registerOptions: { documentSelector: documentSelector }
            });
        }
    }
    register(data) {
        if (!data.registerOptions.documentSelector) {
            return;
        }
        if (!this._listener) {
            this._listener = vscode_1.workspace.onWillSaveTextDocument(this.callback, this);
        }
        this._selectors.set(data.id, data.registerOptions.documentSelector);
    }
    callback(event) {
        if (DocumentNotifications.textDocumentFilter(this._selectors.values(), event.document)) {
            let middleware = this._client.clientOptions.middleware;
            let willSaveWaitUntil = (event) => {
                return this._client.sendRequest(vscode_languageserver_protocol_1.WillSaveTextDocumentWaitUntilRequest.type, this._client.code2ProtocolConverter.asWillSaveTextDocumentParams(event)).then((edits) => {
                    let vEdits = this._client.protocol2CodeConverter.asTextEdits(edits);
                    return vEdits === undefined ? [] : vEdits;
                });
            };
            event.waitUntil(middleware.willSaveWaitUntil
                ? middleware.willSaveWaitUntil(event, willSaveWaitUntil)
                : willSaveWaitUntil(event));
        }
    }
    unregister(id) {
        this._selectors.delete(id);
        if (this._selectors.size === 0 && this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    dispose() {
        this._selectors.clear();
        if (this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
}
class DidSaveTextDocumentFeature extends DocumentNotifications {
    constructor(client) {
        super(client, vscode_1.workspace.onDidSaveTextDocument, vscode_languageserver_protocol_1.DidSaveTextDocumentNotification.type, client.clientOptions.middleware.didSave, (textDocument) => client.code2ProtocolConverter.asSaveTextDocumentParams(textDocument, this._includeText), DocumentNotifications.textDocumentFilter);
        this._includeText = false;
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidSaveTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'synchronization').didSave = true;
    }
    initialize(capabilities, documentSelector) {
        const textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.save) {
            const saveOptions = typeof textDocumentSyncOptions.save === 'boolean'
                ? { includeText: false }
                : { includeText: !!textDocumentSyncOptions.save.includeText };
            this.register({
                id: UUID.generateUuid(),
                registerOptions: Object.assign({}, { documentSelector: documentSelector }, saveOptions)
            });
        }
    }
    register(data) {
        this._includeText = !!data.registerOptions.includeText;
        super.register(data);
    }
}
class FileSystemWatcherFeature {
    constructor(_client, _notifyFileEvent) {
        this._client = _client;
        this._notifyFileEvent = _notifyFileEvent;
        this._watchers = new Map();
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidChangeWatchedFilesNotification.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'workspace'), 'didChangeWatchedFiles').dynamicRegistration = true;
    }
    initialize(_capabilities, _documentSelector) {
    }
    register(data) {
        if (!Array.isArray(data.registerOptions.watchers)) {
            return;
        }
        let disposables = [];
        for (let watcher of data.registerOptions.watchers) {
            if (!Is.string(watcher.globPattern)) {
                continue;
            }
            let watchCreate = true, watchChange = true, watchDelete = true;
            if (watcher.kind !== undefined && watcher.kind !== null) {
                watchCreate = (watcher.kind & vscode_languageserver_protocol_1.WatchKind.Create) !== 0;
                watchChange = (watcher.kind & vscode_languageserver_protocol_1.WatchKind.Change) !== 0;
                watchDelete = (watcher.kind & vscode_languageserver_protocol_1.WatchKind.Delete) !== 0;
            }
            let fileSystemWatcher = vscode_1.workspace.createFileSystemWatcher(watcher.globPattern, !watchCreate, !watchChange, !watchDelete);
            this.hookListeners(fileSystemWatcher, watchCreate, watchChange, watchDelete);
            disposables.push(fileSystemWatcher);
        }
        this._watchers.set(data.id, disposables);
    }
    registerRaw(id, fileSystemWatchers) {
        let disposables = [];
        for (let fileSystemWatcher of fileSystemWatchers) {
            this.hookListeners(fileSystemWatcher, true, true, true, disposables);
        }
        this._watchers.set(id, disposables);
    }
    hookListeners(fileSystemWatcher, watchCreate, watchChange, watchDelete, listeners) {
        if (watchCreate) {
            fileSystemWatcher.onDidCreate((resource) => this._notifyFileEvent({
                uri: this._client.code2ProtocolConverter.asUri(resource),
                type: vscode_languageserver_protocol_1.FileChangeType.Created
            }), null, listeners);
        }
        if (watchChange) {
            fileSystemWatcher.onDidChange((resource) => this._notifyFileEvent({
                uri: this._client.code2ProtocolConverter.asUri(resource),
                type: vscode_languageserver_protocol_1.FileChangeType.Changed
            }), null, listeners);
        }
        if (watchDelete) {
            fileSystemWatcher.onDidDelete((resource) => this._notifyFileEvent({
                uri: this._client.code2ProtocolConverter.asUri(resource),
                type: vscode_languageserver_protocol_1.FileChangeType.Deleted
            }), null, listeners);
        }
    }
    unregister(id) {
        let disposables = this._watchers.get(id);
        if (disposables) {
            for (let disposable of disposables) {
                disposable.dispose();
            }
        }
    }
    dispose() {
        this._watchers.forEach((disposables) => {
            for (let disposable of disposables) {
                disposable.dispose();
            }
        });
        this._watchers.clear();
    }
}
class TextDocumentFeature {
    constructor(_client, _registrationType) {
        this._client = _client;
        this._registrationType = _registrationType;
        this._registrations = new Map();
    }
    get registrationType() {
        return this._registrationType;
    }
    register(data) {
        if (!data.registerOptions.documentSelector) {
            return;
        }
        let registration = this.registerLanguageProvider(data.registerOptions);
        this._registrations.set(data.id, { disposable: registration[0], data, provider: registration[1] });
    }
    unregister(id) {
        let registration = this._registrations.get(id);
        if (registration !== undefined) {
            registration.disposable.dispose();
        }
    }
    dispose() {
        this._registrations.forEach((value) => {
            value.disposable.dispose();
        });
        this._registrations.clear();
    }
    getRegistration(documentSelector, capability) {
        if (!capability) {
            return [undefined, undefined];
        }
        else if (vscode_languageserver_protocol_1.TextDocumentRegistrationOptions.is(capability)) {
            const id = vscode_languageserver_protocol_1.StaticRegistrationOptions.hasId(capability) ? capability.id : UUID.generateUuid();
            const selector = capability.documentSelector || documentSelector;
            if (selector) {
                return [id, Object.assign({}, capability, { documentSelector: selector })];
            }
        }
        else if (Is.boolean(capability) && capability === true || vscode_languageserver_protocol_1.WorkDoneProgressOptions.is(capability)) {
            if (!documentSelector) {
                return [undefined, undefined];
            }
            let options = (Is.boolean(capability) && capability === true ? { documentSelector } : Object.assign({}, capability, { documentSelector }));
            return [UUID.generateUuid(), options];
        }
        return [undefined, undefined];
    }
    getRegistrationOptions(documentSelector, capability) {
        if (!documentSelector || !capability) {
            return undefined;
        }
        return (Is.boolean(capability) && capability === true ? { documentSelector } : Object.assign({}, capability, { documentSelector }));
    }
    getProvider(textDocument) {
        for (const registration of this._registrations.values()) {
            let selector = registration.data.registerOptions.documentSelector;
            if (selector !== null && vscode_1.languages.match(selector, textDocument)) {
                return registration.provider;
            }
        }
        return undefined;
    }
    getAllProviders() {
        const result = [];
        for (const item of this._registrations.values()) {
            result.push(item.provider);
        }
        return result;
    }
}
exports.TextDocumentFeature = TextDocumentFeature;
class WorkspaceFeature {
    constructor(_client, _registrationType) {
        this._client = _client;
        this._registrationType = _registrationType;
        this._registrations = new Map();
    }
    get registrationType() {
        return this._registrationType;
    }
    register(data) {
        const registration = this.registerLanguageProvider(data.registerOptions);
        this._registrations.set(data.id, { disposable: registration[0], provider: registration[1] });
    }
    unregister(id) {
        let registration = this._registrations.get(id);
        if (registration !== undefined) {
            registration.disposable.dispose();
        }
    }
    dispose() {
        this._registrations.forEach((registration) => {
            registration.disposable.dispose();
        });
        this._registrations.clear();
    }
    getProviders() {
        const result = [];
        for (const registration of this._registrations.values()) {
            result.push(registration.provider);
        }
        return result;
    }
}
class CompletionItemFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.CompletionRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let completion = ensure(ensure(capabilities, 'textDocument'), 'completion');
        completion.dynamicRegistration = true;
        completion.contextSupport = true;
        completion.completionItem = {
            snippetSupport: true,
            commitCharactersSupport: true,
            documentationFormat: [vscode_languageserver_protocol_1.MarkupKind.Markdown, vscode_languageserver_protocol_1.MarkupKind.PlainText],
            deprecatedSupport: true,
            preselectSupport: true,
            tagSupport: { valueSet: [vscode_languageserver_protocol_1.CompletionItemTag.Deprecated] },
            insertReplaceSupport: true,
            resolveSupport: {
                properties: ['documentation', 'detail', 'additionalTextEdits']
            },
            insertTextModeSupport: { valueSet: [vscode_languageserver_protocol_1.InsertTextMode.asIs, vscode_languageserver_protocol_1.InsertTextMode.adjustIndentation] }
        };
        completion.completionItemKind = { valueSet: SupportedCompletionItemKinds };
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.completionProvider);
        if (!options) {
            return;
        }
        this.register({
            id: UUID.generateUuid(),
            registerOptions: options
        });
    }
    registerLanguageProvider(options) {
        const triggerCharacters = options.triggerCharacters || [];
        const provider = {
            provideCompletionItems: (document, position, token, context) => {
                const client = this._client;
                const middleware = this._client.clientOptions.middleware;
                const provideCompletionItems = (document, position, context, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.CompletionRequest.type, client.code2ProtocolConverter.asCompletionParams(document, position, context), token).then(client.protocol2CodeConverter.asCompletionResult, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.CompletionRequest.type, error, null);
                    });
                };
                return middleware.provideCompletionItem
                    ? middleware.provideCompletionItem(document, position, context, token, provideCompletionItems)
                    : provideCompletionItems(document, position, context, token);
            },
            resolveCompletionItem: options.resolveProvider
                ? (item, token) => {
                    const client = this._client;
                    const middleware = this._client.clientOptions.middleware;
                    const resolveCompletionItem = (item, token) => {
                        return client.sendRequest(vscode_languageserver_protocol_1.CompletionResolveRequest.type, client.code2ProtocolConverter.asCompletionItem(item), token).then(client.protocol2CodeConverter.asCompletionItem, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.CompletionResolveRequest.type, error, item);
                        });
                    };
                    return middleware.resolveCompletionItem
                        ? middleware.resolveCompletionItem(item, token, resolveCompletionItem)
                        : resolveCompletionItem(item, token);
                }
                : undefined
        };
        return [vscode_1.languages.registerCompletionItemProvider(options.documentSelector, provider, ...triggerCharacters), provider];
    }
}
class HoverFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.HoverRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const hoverCapability = (ensure(ensure(capabilities, 'textDocument'), 'hover'));
        hoverCapability.dynamicRegistration = true;
        hoverCapability.contentFormat = [vscode_languageserver_protocol_1.MarkupKind.Markdown, vscode_languageserver_protocol_1.MarkupKind.PlainText];
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.hoverProvider);
        if (!options) {
            return;
        }
        this.register({
            id: UUID.generateUuid(),
            registerOptions: options
        });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideHover: (document, position, token) => {
                const client = this._client;
                const provideHover = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.HoverRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asHover, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.HoverRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideHover
                    ? middleware.provideHover(document, position, token, provideHover)
                    : provideHover(document, position, token);
            }
        };
        return [vscode_1.languages.registerHoverProvider(options.documentSelector, provider), provider];
    }
}
class SignatureHelpFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.SignatureHelpRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let config = ensure(ensure(capabilities, 'textDocument'), 'signatureHelp');
        config.dynamicRegistration = true;
        config.signatureInformation = { documentationFormat: [vscode_languageserver_protocol_1.MarkupKind.Markdown, vscode_languageserver_protocol_1.MarkupKind.PlainText] };
        config.signatureInformation.parameterInformation = { labelOffsetSupport: true };
        config.signatureInformation.activeParameterSupport = true;
        config.contextSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.signatureHelpProvider);
        if (!options) {
            return;
        }
        this.register({
            id: UUID.generateUuid(),
            registerOptions: options
        });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideSignatureHelp: (document, position, token, context) => {
                const client = this._client;
                const providerSignatureHelp = (document, position, context, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.SignatureHelpRequest.type, client.code2ProtocolConverter.asSignatureHelpParams(document, position, context), token).then(client.protocol2CodeConverter.asSignatureHelp, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.SignatureHelpRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideSignatureHelp
                    ? middleware.provideSignatureHelp(document, position, context, token, providerSignatureHelp)
                    : providerSignatureHelp(document, position, context, token);
            }
        };
        let disposable;
        if (options.retriggerCharacters === undefined) {
            const triggerCharacters = options.triggerCharacters || [];
            disposable = vscode_1.languages.registerSignatureHelpProvider(options.documentSelector, provider, ...triggerCharacters);
        }
        else {
            const metaData = {
                triggerCharacters: options.triggerCharacters || [],
                retriggerCharacters: options.retriggerCharacters || []
            };
            disposable = vscode_1.languages.registerSignatureHelpProvider(options.documentSelector, provider, metaData);
        }
        return [disposable, provider];
    }
}
class DefinitionFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DefinitionRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let definitionSupport = ensure(ensure(capabilities, 'textDocument'), 'definition');
        definitionSupport.dynamicRegistration = true;
        definitionSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.definitionProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideDefinition: (document, position, token) => {
                const client = this._client;
                const provideDefinition = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.DefinitionRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asDefinitionResult, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DefinitionRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideDefinition
                    ? middleware.provideDefinition(document, position, token, provideDefinition)
                    : provideDefinition(document, position, token);
            }
        };
        return [vscode_1.languages.registerDefinitionProvider(options.documentSelector, provider), provider];
    }
}
class ReferencesFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.ReferencesRequest.type);
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'references').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.referencesProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideReferences: (document, position, options, token) => {
                const client = this._client;
                const _providerReferences = (document, position, options, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.ReferencesRequest.type, client.code2ProtocolConverter.asReferenceParams(document, position, options), token).then(client.protocol2CodeConverter.asReferences, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.ReferencesRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideReferences
                    ? middleware.provideReferences(document, position, options, token, _providerReferences)
                    : _providerReferences(document, position, options, token);
            }
        };
        return [vscode_1.languages.registerReferenceProvider(options.documentSelector, provider), provider];
    }
}
class DocumentHighlightFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentHighlightRequest.type);
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'documentHighlight').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentHighlightProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideDocumentHighlights: (document, position, token) => {
                const client = this._client;
                const _provideDocumentHighlights = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentHighlightRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asDocumentHighlights, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentHighlightRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideDocumentHighlights
                    ? middleware.provideDocumentHighlights(document, position, token, _provideDocumentHighlights)
                    : _provideDocumentHighlights(document, position, token);
            }
        };
        return [vscode_1.languages.registerDocumentHighlightProvider(options.documentSelector, provider), provider];
    }
}
class DocumentSymbolFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentSymbolRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let symbolCapabilities = ensure(ensure(capabilities, 'textDocument'), 'documentSymbol');
        symbolCapabilities.dynamicRegistration = true;
        symbolCapabilities.symbolKind = {
            valueSet: SupportedSymbolKinds
        };
        symbolCapabilities.hierarchicalDocumentSymbolSupport = true;
        symbolCapabilities.tagSupport = {
            valueSet: SupportedSymbolTags
        };
        symbolCapabilities.labelSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentSymbolProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideDocumentSymbols: (document, token) => {
                const client = this._client;
                const _provideDocumentSymbols = (document, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentSymbolRequest.type, client.code2ProtocolConverter.asDocumentSymbolParams(document), token).then((data) => {
                        if (data === null) {
                            return undefined;
                        }
                        if (data.length === 0) {
                            return [];
                        }
                        else {
                            let element = data[0];
                            if (vscode_languageserver_protocol_1.DocumentSymbol.is(element)) {
                                return client.protocol2CodeConverter.asDocumentSymbols(data);
                            }
                            else {
                                return client.protocol2CodeConverter.asSymbolInformations(data);
                            }
                        }
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentSymbolRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideDocumentSymbols
                    ? middleware.provideDocumentSymbols(document, token, _provideDocumentSymbols)
                    : _provideDocumentSymbols(document, token);
            }
        };
        const metaData = options.label !== undefined ? { label: options.label } : undefined;
        return [vscode_1.languages.registerDocumentSymbolProvider(options.documentSelector, provider, metaData), provider];
    }
}
class WorkspaceSymbolFeature extends WorkspaceFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.WorkspaceSymbolRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let symbolCapabilities = ensure(ensure(capabilities, 'workspace'), 'symbol');
        symbolCapabilities.dynamicRegistration = true;
        symbolCapabilities.symbolKind = {
            valueSet: SupportedSymbolKinds
        };
        symbolCapabilities.tagSupport = {
            valueSet: SupportedSymbolTags
        };
    }
    initialize(capabilities) {
        if (!capabilities.workspaceSymbolProvider) {
            return;
        }
        this.register({
            id: UUID.generateUuid(),
            registerOptions: capabilities.workspaceSymbolProvider === true ? { workDoneProgress: false } : capabilities.workspaceSymbolProvider
        });
    }
    registerLanguageProvider(_options) {
        const provider = {
            provideWorkspaceSymbols: (query, token) => {
                const client = this._client;
                const provideWorkspaceSymbols = (query, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.WorkspaceSymbolRequest.type, { query }, token).then(client.protocol2CodeConverter.asSymbolInformations, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.WorkspaceSymbolRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideWorkspaceSymbols
                    ? middleware.provideWorkspaceSymbols(query, token, provideWorkspaceSymbols)
                    : provideWorkspaceSymbols(query, token);
            }
        };
        return [vscode_1.languages.registerWorkspaceSymbolProvider(provider), provider];
    }
}
class CodeActionFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.CodeActionRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const cap = ensure(ensure(capabilities, 'textDocument'), 'codeAction');
        cap.dynamicRegistration = true;
        cap.isPreferredSupport = true;
        cap.disabledSupport = true;
        cap.dataSupport = true;
        // We can only resolve the edit property.
        cap.resolveSupport = {
            properties: ['edit']
        };
        cap.codeActionLiteralSupport = {
            codeActionKind: {
                valueSet: [
                    vscode_languageserver_protocol_1.CodeActionKind.Empty,
                    vscode_languageserver_protocol_1.CodeActionKind.QuickFix,
                    vscode_languageserver_protocol_1.CodeActionKind.Refactor,
                    vscode_languageserver_protocol_1.CodeActionKind.RefactorExtract,
                    vscode_languageserver_protocol_1.CodeActionKind.RefactorInline,
                    vscode_languageserver_protocol_1.CodeActionKind.RefactorRewrite,
                    vscode_languageserver_protocol_1.CodeActionKind.Source,
                    vscode_languageserver_protocol_1.CodeActionKind.SourceOrganizeImports
                ]
            }
        };
        cap.honorsChangeAnnotations = false;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.codeActionProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideCodeActions: (document, range, context, token) => {
                const client = this._client;
                const _provideCodeActions = (document, range, context, token) => {
                    const params = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        range: client.code2ProtocolConverter.asRange(range),
                        context: client.code2ProtocolConverter.asCodeActionContext(context)
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.CodeActionRequest.type, params, token).then((values) => {
                        if (values === null) {
                            return undefined;
                        }
                        const result = [];
                        for (let item of values) {
                            if (vscode_languageserver_protocol_1.Command.is(item)) {
                                result.push(client.protocol2CodeConverter.asCommand(item));
                            }
                            else {
                                result.push(client.protocol2CodeConverter.asCodeAction(item));
                            }
                        }
                        return result;
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.CodeActionRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideCodeActions
                    ? middleware.provideCodeActions(document, range, context, token, _provideCodeActions)
                    : _provideCodeActions(document, range, context, token);
            },
            resolveCodeAction: options.resolveProvider
                ? (item, token) => {
                    const client = this._client;
                    const middleware = this._client.clientOptions.middleware;
                    const resolveCodeAction = (item, token) => {
                        return client.sendRequest(vscode_languageserver_protocol_1.CodeActionResolveRequest.type, client.code2ProtocolConverter.asCodeAction(item), token).then(client.protocol2CodeConverter.asCodeAction, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.CodeActionResolveRequest.type, error, item);
                        });
                    };
                    return middleware.resolveCodeAction
                        ? middleware.resolveCodeAction(item, token, resolveCodeAction)
                        : resolveCodeAction(item, token);
                }
                : undefined
        };
        return [vscode_1.languages.registerCodeActionsProvider(options.documentSelector, provider, (options.codeActionKinds
                ? { providedCodeActionKinds: this._client.protocol2CodeConverter.asCodeActionKinds(options.codeActionKinds) }
                : undefined)), provider];
    }
}
class CodeLensFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.CodeLensRequest.type);
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'codeLens').dynamicRegistration = true;
        ensure(ensure(capabilities, 'workspace'), 'codeLens').refreshSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const client = this._client;
        client.onRequest(vscode_languageserver_protocol_1.CodeLensRefreshRequest.type, async () => {
            for (const provider of this.getAllProviders()) {
                provider.onDidChangeCodeLensEmitter.fire();
            }
        });
        const options = this.getRegistrationOptions(documentSelector, capabilities.codeLensProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const eventEmitter = new vscode_1.EventEmitter();
        const provider = {
            onDidChangeCodeLenses: eventEmitter.event,
            provideCodeLenses: (document, token) => {
                const client = this._client;
                const provideCodeLenses = (document, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.CodeLensRequest.type, client.code2ProtocolConverter.asCodeLensParams(document), token).then(client.protocol2CodeConverter.asCodeLenses, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.CodeLensRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideCodeLenses
                    ? middleware.provideCodeLenses(document, token, provideCodeLenses)
                    : provideCodeLenses(document, token);
            },
            resolveCodeLens: (options.resolveProvider)
                ? (codeLens, token) => {
                    const client = this._client;
                    const resolveCodeLens = (codeLens, token) => {
                        return client.sendRequest(vscode_languageserver_protocol_1.CodeLensResolveRequest.type, client.code2ProtocolConverter.asCodeLens(codeLens), token).then(client.protocol2CodeConverter.asCodeLens, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.CodeLensResolveRequest.type, error, codeLens);
                        });
                    };
                    const middleware = client.clientOptions.middleware;
                    return middleware.resolveCodeLens
                        ? middleware.resolveCodeLens(codeLens, token, resolveCodeLens)
                        : resolveCodeLens(codeLens, token);
                }
                : undefined
        };
        return [vscode_1.languages.registerCodeLensProvider(options.documentSelector, provider), { provider, onDidChangeCodeLensEmitter: eventEmitter }];
    }
}
class DocumentFormattingFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentFormattingRequest.type);
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'formatting').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentFormattingProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideDocumentFormattingEdits: (document, options, token) => {
                const client = this._client;
                const provideDocumentFormattingEdits = (document, options, token) => {
                    const params = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        options: client.code2ProtocolConverter.asFormattingOptions(options, FileFormattingOptions.fromConfiguration(document))
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentFormattingRequest.type, params, token).then(client.protocol2CodeConverter.asTextEdits, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentFormattingRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideDocumentFormattingEdits
                    ? middleware.provideDocumentFormattingEdits(document, options, token, provideDocumentFormattingEdits)
                    : provideDocumentFormattingEdits(document, options, token);
            }
        };
        return [vscode_1.languages.registerDocumentFormattingEditProvider(options.documentSelector, provider), provider];
    }
}
class DocumentRangeFormattingFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentRangeFormattingRequest.type);
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'rangeFormatting').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentRangeFormattingProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideDocumentRangeFormattingEdits: (document, range, options, token) => {
                const client = this._client;
                const provideDocumentRangeFormattingEdits = (document, range, options, token) => {
                    const params = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        range: client.code2ProtocolConverter.asRange(range),
                        options: client.code2ProtocolConverter.asFormattingOptions(options, FileFormattingOptions.fromConfiguration(document))
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentRangeFormattingRequest.type, params, token).then(client.protocol2CodeConverter.asTextEdits, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentRangeFormattingRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideDocumentRangeFormattingEdits
                    ? middleware.provideDocumentRangeFormattingEdits(document, range, options, token, provideDocumentRangeFormattingEdits)
                    : provideDocumentRangeFormattingEdits(document, range, options, token);
            }
        };
        return [vscode_1.languages.registerDocumentRangeFormattingEditProvider(options.documentSelector, provider), provider];
    }
}
class DocumentOnTypeFormattingFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentOnTypeFormattingRequest.type);
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'onTypeFormatting').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentOnTypeFormattingProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideOnTypeFormattingEdits: (document, position, ch, options, token) => {
                const client = this._client;
                const provideOnTypeFormattingEdits = (document, position, ch, options, token) => {
                    let params = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        position: client.code2ProtocolConverter.asPosition(position),
                        ch: ch,
                        options: client.code2ProtocolConverter.asFormattingOptions(options, FileFormattingOptions.fromConfiguration(document))
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentOnTypeFormattingRequest.type, params, token).then(client.protocol2CodeConverter.asTextEdits, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentOnTypeFormattingRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideOnTypeFormattingEdits
                    ? middleware.provideOnTypeFormattingEdits(document, position, ch, options, token, provideOnTypeFormattingEdits)
                    : provideOnTypeFormattingEdits(document, position, ch, options, token);
            }
        };
        const moreTriggerCharacter = options.moreTriggerCharacter || [];
        return [vscode_1.languages.registerOnTypeFormattingEditProvider(options.documentSelector, provider, options.firstTriggerCharacter, ...moreTriggerCharacter), provider];
    }
}
class RenameFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.RenameRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let rename = ensure(ensure(capabilities, 'textDocument'), 'rename');
        rename.dynamicRegistration = true;
        rename.prepareSupport = true;
        rename.prepareSupportDefaultBehavior = vscode_languageserver_protocol_1.PrepareSupportDefaultBehavior.Identifier;
        rename.honorsChangeAnnotations = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.renameProvider);
        if (!options) {
            return;
        }
        if (Is.boolean(capabilities.renameProvider)) {
            options.prepareProvider = false;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideRenameEdits: (document, position, newName, token) => {
                const client = this._client;
                const provideRenameEdits = (document, position, newName, token) => {
                    let params = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        position: client.code2ProtocolConverter.asPosition(position),
                        newName: newName
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.RenameRequest.type, params, token).then(client.protocol2CodeConverter.asWorkspaceEdit, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.RenameRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideRenameEdits
                    ? middleware.provideRenameEdits(document, position, newName, token, provideRenameEdits)
                    : provideRenameEdits(document, position, newName, token);
            },
            prepareRename: options.prepareProvider
                ? (document, position, token) => {
                    const client = this._client;
                    const prepareRename = (document, position, token) => {
                        let params = {
                            textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                            position: client.code2ProtocolConverter.asPosition(position),
                        };
                        return client.sendRequest(vscode_languageserver_protocol_1.PrepareRenameRequest.type, params, token).then((result) => {
                            if (vscode_languageserver_protocol_1.Range.is(result)) {
                                return client.protocol2CodeConverter.asRange(result);
                            }
                            else if (this.isDefaultBehavior(result)) {
                                return result.defaultBehavior === true
                                    ? null
                                    : Promise.reject(new Error(`The element can't be renamed.`));
                            }
                            else if (result && vscode_languageserver_protocol_1.Range.is(result.range)) {
                                return {
                                    range: client.protocol2CodeConverter.asRange(result.range),
                                    placeholder: result.placeholder
                                };
                            }
                            // To cancel the rename vscode API expects a rejected promise.
                            return Promise.reject(new Error(`The element can't be renamed.`));
                        }, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.PrepareRenameRequest.type, error, undefined);
                        });
                    };
                    const middleware = client.clientOptions.middleware;
                    return middleware.prepareRename
                        ? middleware.prepareRename(document, position, token, prepareRename)
                        : prepareRename(document, position, token);
                }
                : undefined
        };
        return [vscode_1.languages.registerRenameProvider(options.documentSelector, provider), provider];
    }
    isDefaultBehavior(value) {
        const candidate = value;
        return candidate && Is.boolean(candidate.defaultBehavior);
    }
}
class DocumentLinkFeature extends TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentLinkRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const documentLinkCapabilities = ensure(ensure(capabilities, 'textDocument'), 'documentLink');
        documentLinkCapabilities.dynamicRegistration = true;
        documentLinkCapabilities.tooltipSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentLinkProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideDocumentLinks: (document, token) => {
                const client = this._client;
                const provideDocumentLinks = (document, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentLinkRequest.type, client.code2ProtocolConverter.asDocumentLinkParams(document), token).then(client.protocol2CodeConverter.asDocumentLinks, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentLinkRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideDocumentLinks
                    ? middleware.provideDocumentLinks(document, token, provideDocumentLinks)
                    : provideDocumentLinks(document, token);
            },
            resolveDocumentLink: options.resolveProvider
                ? (link, token) => {
                    const client = this._client;
                    let resolveDocumentLink = (link, token) => {
                        return client.sendRequest(vscode_languageserver_protocol_1.DocumentLinkResolveRequest.type, client.code2ProtocolConverter.asDocumentLink(link), token).then(client.protocol2CodeConverter.asDocumentLink, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentLinkResolveRequest.type, error, link);
                        });
                    };
                    const middleware = client.clientOptions.middleware;
                    return middleware.resolveDocumentLink
                        ? middleware.resolveDocumentLink(link, token, resolveDocumentLink)
                        : resolveDocumentLink(link, token);
                }
                : undefined
        };
        return [vscode_1.languages.registerDocumentLinkProvider(options.documentSelector, provider), provider];
    }
}
class ConfigurationFeature {
    constructor(_client) {
        this._client = _client;
        this._listeners = new Map();
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'workspace'), 'didChangeConfiguration').dynamicRegistration = true;
    }
    initialize() {
        let section = this._client.clientOptions.synchronize.configurationSection;
        if (section !== undefined) {
            this.register({
                id: UUID.generateUuid(),
                registerOptions: {
                    section: section
                }
            });
        }
    }
    register(data) {
        let disposable = vscode_1.workspace.onDidChangeConfiguration((event) => {
            this.onDidChangeConfiguration(data.registerOptions.section, event);
        });
        this._listeners.set(data.id, disposable);
        if (data.registerOptions.section !== undefined) {
            this.onDidChangeConfiguration(data.registerOptions.section, undefined);
        }
    }
    unregister(id) {
        let disposable = this._listeners.get(id);
        if (disposable) {
            this._listeners.delete(id);
            disposable.dispose();
        }
    }
    dispose() {
        for (let disposable of this._listeners.values()) {
            disposable.dispose();
        }
        this._listeners.clear();
    }
    onDidChangeConfiguration(configurationSection, event) {
        let sections;
        if (Is.string(configurationSection)) {
            sections = [configurationSection];
        }
        else {
            sections = configurationSection;
        }
        if (sections !== undefined && event !== undefined) {
            let affected = sections.some((section) => event.affectsConfiguration(section));
            if (!affected) {
                return;
            }
        }
        let didChangeConfiguration = (sections) => {
            if (sections === undefined) {
                this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type, { settings: null });
                return;
            }
            this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type, { settings: this.extractSettingsInformation(sections) });
        };
        let middleware = this.getMiddleware();
        middleware
            ? middleware(sections, didChangeConfiguration)
            : didChangeConfiguration(sections);
    }
    extractSettingsInformation(keys) {
        function ensurePath(config, path) {
            let current = config;
            for (let i = 0; i < path.length - 1; i++) {
                let obj = current[path[i]];
                if (!obj) {
                    obj = Object.create(null);
                    current[path[i]] = obj;
                }
                current = obj;
            }
            return current;
        }
        let resource = this._client.clientOptions.workspaceFolder
            ? this._client.clientOptions.workspaceFolder.uri
            : undefined;
        let result = Object.create(null);
        for (let i = 0; i < keys.length; i++) {
            let key = keys[i];
            let index = key.indexOf('.');
            let config = null;
            if (index >= 0) {
                config = vscode_1.workspace.getConfiguration(key.substr(0, index), resource).get(key.substr(index + 1));
            }
            else {
                config = vscode_1.workspace.getConfiguration(undefined, resource).get(key);
            }
            if (config) {
                let path = keys[i].split('.');
                ensurePath(result, path)[path[path.length - 1]] = configuration_1.toJSONObject(config);
            }
        }
        return result;
    }
    getMiddleware() {
        let middleware = this._client.clientOptions.middleware;
        if (middleware.workspace && middleware.workspace.didChangeConfiguration) {
            return middleware.workspace.didChangeConfiguration;
        }
        else {
            return undefined;
        }
    }
}
class ExecuteCommandFeature {
    constructor(_client) {
        this._client = _client;
        this._commands = new Map();
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.ExecuteCommandRequest.type;
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'workspace'), 'executeCommand').dynamicRegistration = true;
    }
    initialize(capabilities) {
        if (!capabilities.executeCommandProvider) {
            return;
        }
        this.register({
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, capabilities.executeCommandProvider)
        });
    }
    register(data) {
        const client = this._client;
        const middleware = client.clientOptions.middleware;
        const executeCommand = (command, args) => {
            let params = {
                command,
                arguments: args
            };
            return client.sendRequest(vscode_languageserver_protocol_1.ExecuteCommandRequest.type, params).then(undefined, (error) => {
                return client.handleFailedRequest(vscode_languageserver_protocol_1.ExecuteCommandRequest.type, error, undefined);
            });
        };
        if (data.registerOptions.commands) {
            const disposables = [];
            for (const command of data.registerOptions.commands) {
                disposables.push(vscode_1.commands.registerCommand(command, (...args) => {
                    return middleware.executeCommand
                        ? middleware.executeCommand(command, args, executeCommand)
                        : executeCommand(command, args);
                }));
            }
            this._commands.set(data.id, disposables);
        }
    }
    unregister(id) {
        let disposables = this._commands.get(id);
        if (disposables) {
            disposables.forEach(disposable => disposable.dispose());
        }
    }
    dispose() {
        this._commands.forEach((value) => {
            value.forEach(disposable => disposable.dispose());
        });
        this._commands.clear();
    }
}
var MessageTransports;
(function (MessageTransports) {
    function is(value) {
        let candidate = value;
        return candidate && vscode_languageserver_protocol_1.MessageReader.is(value.reader) && vscode_languageserver_protocol_1.MessageWriter.is(value.writer);
    }
    MessageTransports.is = is;
})(MessageTransports = exports.MessageTransports || (exports.MessageTransports = {}));
class OnReady {
    constructor(_resolve, _reject) {
        this._resolve = _resolve;
        this._reject = _reject;
        this._used = false;
    }
    get isUsed() {
        return this._used;
    }
    resolve() {
        this._used = true;
        this._resolve();
    }
    reject(error) {
        this._used = true;
        this._reject(error);
    }
}
class BaseLanguageClient {
    constructor(id, name, clientOptions) {
        var _a;
        this._traceFormat = vscode_languageserver_protocol_1.TraceFormat.Text;
        this._features = [];
        this._dynamicFeatures = new Map();
        this._id = id;
        this._name = name;
        clientOptions = clientOptions || {};
        const markdown = { isTrusted: false };
        if (clientOptions.markdown !== undefined && clientOptions.markdown.isTrusted === true) {
            markdown.isTrusted = true;
        }
        this._clientOptions = {
            documentSelector: clientOptions.documentSelector || [],
            synchronize: clientOptions.synchronize || {},
            diagnosticCollectionName: clientOptions.diagnosticCollectionName,
            outputChannelName: clientOptions.outputChannelName || this._name,
            revealOutputChannelOn: clientOptions.revealOutputChannelOn || RevealOutputChannelOn.Error,
            stdioEncoding: clientOptions.stdioEncoding || 'utf8',
            initializationOptions: clientOptions.initializationOptions,
            initializationFailedHandler: clientOptions.initializationFailedHandler,
            progressOnInitialization: !!clientOptions.progressOnInitialization,
            errorHandler: clientOptions.errorHandler || this.createDefaultErrorHandler((_a = clientOptions.connectionOptions) === null || _a === void 0 ? void 0 : _a.maxRestartCount),
            middleware: clientOptions.middleware || {},
            uriConverters: clientOptions.uriConverters,
            workspaceFolder: clientOptions.workspaceFolder,
            connectionOptions: clientOptions.connectionOptions,
            markdown
        };
        this._clientOptions.synchronize = this._clientOptions.synchronize || {};
        this._state = ClientState.Initial;
        this._connectionPromise = undefined;
        this._resolvedConnection = undefined;
        this._initializeResult = undefined;
        if (clientOptions.outputChannel) {
            this._outputChannel = clientOptions.outputChannel;
            this._disposeOutputChannel = false;
        }
        else {
            this._outputChannel = undefined;
            this._disposeOutputChannel = true;
        }
        this._traceOutputChannel = clientOptions.traceOutputChannel;
        this._listeners = undefined;
        this._providers = undefined;
        this._diagnostics = undefined;
        this._fileEvents = [];
        this._fileEventDelayer = new async_1.Delayer(250);
        this._onReady = new Promise((resolve, reject) => {
            this._onReadyCallbacks = new OnReady(resolve, reject);
        });
        this._onStop = undefined;
        this._telemetryEmitter = new vscode_languageserver_protocol_1.Emitter();
        this._stateChangeEmitter = new vscode_languageserver_protocol_1.Emitter();
        this._trace = vscode_languageserver_protocol_1.Trace.Off;
        this._tracer = {
            log: (messageOrDataObject, data) => {
                if (Is.string(messageOrDataObject)) {
                    this.logTrace(messageOrDataObject, data);
                }
                else {
                    this.logObjectTrace(messageOrDataObject);
                }
            },
        };
        this._c2p = c2p.createConverter(clientOptions.uriConverters ? clientOptions.uriConverters.code2Protocol : undefined);
        this._p2c = p2c.createConverter(clientOptions.uriConverters ? clientOptions.uriConverters.protocol2Code : undefined, this._clientOptions.markdown.isTrusted);
        this._syncedDocuments = new Map();
        this.registerBuiltinFeatures();
    }
    get state() {
        return this._state;
    }
    set state(value) {
        let oldState = this.getPublicState();
        this._state = value;
        let newState = this.getPublicState();
        if (newState !== oldState) {
            this._stateChangeEmitter.fire({ oldState, newState });
        }
    }
    getPublicState() {
        if (this.state === ClientState.Running) {
            return State.Running;
        }
        else if (this.state === ClientState.Starting) {
            return State.Starting;
        }
        else {
            return State.Stopped;
        }
    }
    get initializeResult() {
        return this._initializeResult;
    }
    sendRequest(type, ...params) {
        if (!this.isConnectionActive()) {
            throw new Error('Language client is not ready yet');
        }
        this.forceDocumentSync();
        try {
            return this._resolvedConnection.sendRequest(type, ...params);
        }
        catch (error) {
            this.error(`Sending request ${Is.string(type) ? type : type.method} failed.`, error);
            throw error;
        }
    }
    onRequest(type, handler) {
        if (!this.isConnectionActive()) {
            throw new Error('Language client is not ready yet');
        }
        try {
            return this._resolvedConnection.onRequest(type, handler);
        }
        catch (error) {
            this.error(`Registering request handler ${Is.string(type) ? type : type.method} failed.`, error);
            throw error;
        }
    }
    sendNotification(type, params) {
        if (!this.isConnectionActive()) {
            throw new Error('Language client is not ready yet');
        }
        this.forceDocumentSync();
        try {
            this._resolvedConnection.sendNotification(type, params);
        }
        catch (error) {
            this.error(`Sending notification ${Is.string(type) ? type : type.method} failed.`, error);
            throw error;
        }
    }
    onNotification(type, handler) {
        if (!this.isConnectionActive()) {
            throw new Error('Language client is not ready yet');
        }
        try {
            return this._resolvedConnection.onNotification(type, handler);
        }
        catch (error) {
            this.error(`Registering notification handler ${Is.string(type) ? type : type.method} failed.`, error);
            throw error;
        }
    }
    onProgress(type, token, handler) {
        if (!this.isConnectionActive()) {
            throw new Error('Language client is not ready yet');
        }
        try {
            if (vscode_languageserver_protocol_1.WorkDoneProgress.is(type)) {
                const handleWorkDoneProgress = this._clientOptions.middleware.handleWorkDoneProgress;
                if (handleWorkDoneProgress !== undefined) {
                    return this._resolvedConnection.onProgress(type, token, (params) => {
                        handleWorkDoneProgress(token, params, () => handler(params));
                    });
                }
            }
            return this._resolvedConnection.onProgress(type, token, handler);
        }
        catch (error) {
            this.error(`Registering progress handler for token ${token} failed.`, error);
            throw error;
        }
    }
    sendProgress(type, token, value) {
        if (!this.isConnectionActive()) {
            throw new Error('Language client is not ready yet');
        }
        this.forceDocumentSync();
        try {
            this._resolvedConnection.sendProgress(type, token, value);
        }
        catch (error) {
            this.error(`Sending progress for token ${token} failed.`, error);
            throw error;
        }
    }
    get clientOptions() {
        return this._clientOptions;
    }
    get protocol2CodeConverter() {
        return this._p2c;
    }
    get code2ProtocolConverter() {
        return this._c2p;
    }
    get onTelemetry() {
        return this._telemetryEmitter.event;
    }
    get onDidChangeState() {
        return this._stateChangeEmitter.event;
    }
    get outputChannel() {
        if (!this._outputChannel) {
            this._outputChannel = vscode_1.window.createOutputChannel(this._clientOptions.outputChannelName ? this._clientOptions.outputChannelName : this._name);
        }
        return this._outputChannel;
    }
    get traceOutputChannel() {
        if (this._traceOutputChannel) {
            return this._traceOutputChannel;
        }
        return this.outputChannel;
    }
    get diagnostics() {
        return this._diagnostics;
    }
    createDefaultErrorHandler(maxRestartCount) {
        if (maxRestartCount !== undefined && maxRestartCount < 0) {
            throw new Error(`Invalid maxRestartCount: ${maxRestartCount}`);
        }
        return new DefaultErrorHandler(this._name, maxRestartCount !== null && maxRestartCount !== void 0 ? maxRestartCount : 4);
    }
    set trace(value) {
        this._trace = value;
        this.onReady().then(() => {
            this.resolveConnection().then((connection) => {
                connection.trace(this._trace, this._tracer, {
                    sendNotification: false,
                    traceFormat: this._traceFormat
                });
            });
        }, () => {
        });
    }
    data2String(data) {
        if (data instanceof vscode_languageserver_protocol_1.ResponseError) {
            const responseError = data;
            return `  Message: ${responseError.message}\n  Code: ${responseError.code} ${responseError.data ? '\n' + responseError.data.toString() : ''}`;
        }
        if (data instanceof Error) {
            if (Is.string(data.stack)) {
                return data.stack;
            }
            return data.message;
        }
        if (Is.string(data)) {
            return data;
        }
        return data.toString();
    }
    info(message, data, showNotification = true) {
        this.outputChannel.appendLine(`[Info  - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data) {
            this.outputChannel.appendLine(this.data2String(data));
        }
        if (showNotification && this._clientOptions.revealOutputChannelOn <= RevealOutputChannelOn.Info) {
            this.showNotificationMessage();
        }
    }
    warn(message, data, showNotification = true) {
        this.outputChannel.appendLine(`[Warn  - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data) {
            this.outputChannel.appendLine(this.data2String(data));
        }
        if (showNotification && this._clientOptions.revealOutputChannelOn <= RevealOutputChannelOn.Warn) {
            this.showNotificationMessage();
        }
    }
    error(message, data, showNotification = true) {
        this.outputChannel.appendLine(`[Error - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data) {
            this.outputChannel.appendLine(this.data2String(data));
        }
        if (showNotification && this._clientOptions.revealOutputChannelOn <= RevealOutputChannelOn.Error) {
            this.showNotificationMessage();
        }
    }
    showNotificationMessage() {
        vscode_1.window.showInformationMessage('A request has failed. See the output for more information.', 'Go to output').then(() => {
            this.outputChannel.show(true);
        });
    }
    logTrace(message, data) {
        this.traceOutputChannel.appendLine(`[Trace - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data) {
            this.traceOutputChannel.appendLine(this.data2String(data));
        }
    }
    logObjectTrace(data) {
        if (data.isLSPMessage && data.type) {
            this.traceOutputChannel.append(`[LSP   - ${(new Date().toLocaleTimeString())}] `);
        }
        else {
            this.traceOutputChannel.append(`[Trace - ${(new Date().toLocaleTimeString())}] `);
        }
        if (data) {
            this.traceOutputChannel.appendLine(`${JSON.stringify(data)}`);
        }
    }
    needsStart() {
        return this.state === ClientState.Initial || this.state === ClientState.Stopping || this.state === ClientState.Stopped;
    }
    needsStop() {
        return this.state === ClientState.Starting || this.state === ClientState.Running;
    }
    onReady() {
        return this._onReady;
    }
    isConnectionActive() {
        return this.state === ClientState.Running && !!this._resolvedConnection;
    }
    start() {
        if (this._onReadyCallbacks.isUsed) {
            this._onReady = new Promise((resolve, reject) => {
                this._onReadyCallbacks = new OnReady(resolve, reject);
            });
        }
        this._listeners = [];
        this._providers = [];
        // If we restart then the diagnostics collection is reused.
        if (!this._diagnostics) {
            this._diagnostics = this._clientOptions.diagnosticCollectionName
                ? vscode_1.languages.createDiagnosticCollection(this._clientOptions.diagnosticCollectionName)
                : vscode_1.languages.createDiagnosticCollection();
        }
        this.state = ClientState.Starting;
        this.resolveConnection().then((connection) => {
            connection.onLogMessage((message) => {
                switch (message.type) {
                    case vscode_languageserver_protocol_1.MessageType.Error:
                        this.error(message.message, undefined, false);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Warning:
                        this.warn(message.message, undefined, false);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Info:
                        this.info(message.message, undefined, false);
                        break;
                    default:
                        this.outputChannel.appendLine(message.message);
                }
            });
            connection.onShowMessage((message) => {
                switch (message.type) {
                    case vscode_languageserver_protocol_1.MessageType.Error:
                        vscode_1.window.showErrorMessage(message.message);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Warning:
                        vscode_1.window.showWarningMessage(message.message);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Info:
                        vscode_1.window.showInformationMessage(message.message);
                        break;
                    default:
                        vscode_1.window.showInformationMessage(message.message);
                }
            });
            connection.onRequest(vscode_languageserver_protocol_1.ShowMessageRequest.type, (params) => {
                let messageFunc;
                switch (params.type) {
                    case vscode_languageserver_protocol_1.MessageType.Error:
                        messageFunc = vscode_1.window.showErrorMessage;
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Warning:
                        messageFunc = vscode_1.window.showWarningMessage;
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Info:
                        messageFunc = vscode_1.window.showInformationMessage;
                        break;
                    default:
                        messageFunc = vscode_1.window.showInformationMessage;
                }
                let actions = params.actions || [];
                return messageFunc(params.message, ...actions);
            });
            connection.onTelemetry((data) => {
                this._telemetryEmitter.fire(data);
            });
            connection.onRequest(vscode_languageserver_protocol_1.ShowDocumentRequest.type, async (params) => {
                var _a;
                const showDocument = async (params) => {
                    const uri = this.protocol2CodeConverter.asUri(params.uri);
                    try {
                        if (params.external === true) {
                            const success = await vscode_1.env.openExternal(uri);
                            return { success };
                        }
                        else {
                            const options = {};
                            if (params.selection !== undefined) {
                                options.selection = this.protocol2CodeConverter.asRange(params.selection);
                            }
                            if (params.takeFocus === undefined || params.takeFocus === false) {
                                options.preserveFocus = true;
                            }
                            else if (params.takeFocus === true) {
                                options.preserveFocus = false;
                            }
                            await vscode_1.window.showTextDocument(uri, options);
                            return { success: true };
                        }
                    }
                    catch (error) {
                        return { success: true };
                    }
                };
                const middleware = (_a = this._clientOptions.middleware.window) === null || _a === void 0 ? void 0 : _a.showDocument;
                if (middleware !== undefined) {
                    return middleware(params, showDocument);
                }
                else {
                    return showDocument(params);
                }
            });
            connection.listen();
            // Error is handled in the initialize call.
            return this.initialize(connection);
        }).then(undefined, (error) => {
            this.state = ClientState.StartFailed;
            this._onReadyCallbacks.reject(error);
            this.error('Starting client failed', error);
            vscode_1.window.showErrorMessage(`Couldn't start client ${this._name}`);
        });
        return new vscode_1.Disposable(() => {
            if (this.needsStop()) {
                this.stop();
            }
        });
    }
    resolveConnection() {
        if (!this._connectionPromise) {
            this._connectionPromise = this.createConnection();
        }
        return this._connectionPromise;
    }
    initialize(connection) {
        this.refreshTrace(connection, false);
        let initOption = this._clientOptions.initializationOptions;
        let rootPath = this._clientOptions.workspaceFolder
            ? this._clientOptions.workspaceFolder.uri.fsPath
            : this._clientGetRootPath();
        let initParams = {
            processId: null,
            clientInfo: {
                name: vscode_1.env.appName,
                version: vscode_1.version
            },
            locale: this.getLocale(),
            rootPath: rootPath ? rootPath : null,
            rootUri: rootPath ? this._c2p.asUri(vscode_1.Uri.file(rootPath)) : null,
            capabilities: this.computeClientCapabilities(),
            initializationOptions: Is.func(initOption) ? initOption() : initOption,
            trace: vscode_languageserver_protocol_1.Trace.toString(this._trace),
            workspaceFolders: null
        };
        this.fillInitializeParams(initParams);
        if (this._clientOptions.progressOnInitialization) {
            const token = UUID.generateUuid();
            const part = new progressPart_1.ProgressPart(connection, token);
            initParams.workDoneToken = token;
            return this.doInitialize(connection, initParams).then((result) => {
                part.done();
                return result;
            }, (error) => {
                part.cancel();
                throw error;
            });
        }
        else {
            return this.doInitialize(connection, initParams);
        }
    }
    doInitialize(connection, initParams) {
        return connection.initialize(initParams).then((result) => {
            this._resolvedConnection = connection;
            this._initializeResult = result;
            this.state = ClientState.Running;
            let textDocumentSyncOptions = undefined;
            if (Is.number(result.capabilities.textDocumentSync)) {
                if (result.capabilities.textDocumentSync === vscode_languageserver_protocol_1.TextDocumentSyncKind.None) {
                    textDocumentSyncOptions = {
                        openClose: false,
                        change: vscode_languageserver_protocol_1.TextDocumentSyncKind.None,
                        save: undefined
                    };
                }
                else {
                    textDocumentSyncOptions = {
                        openClose: true,
                        change: result.capabilities.textDocumentSync,
                        save: {
                            includeText: false
                        }
                    };
                }
            }
            else if (result.capabilities.textDocumentSync !== undefined && result.capabilities.textDocumentSync !== null) {
                textDocumentSyncOptions = result.capabilities.textDocumentSync;
            }
            this._capabilities = Object.assign({}, result.capabilities, { resolvedTextDocumentSync: textDocumentSyncOptions });
            connection.onDiagnostics(params => this.handleDiagnostics(params));
            connection.onRequest(vscode_languageserver_protocol_1.RegistrationRequest.type, params => this.handleRegistrationRequest(params));
            // See https://github.com/Microsoft/vscode-languageserver-node/issues/199
            connection.onRequest('client/registerFeature', params => this.handleRegistrationRequest(params));
            connection.onRequest(vscode_languageserver_protocol_1.UnregistrationRequest.type, params => this.handleUnregistrationRequest(params));
            // See https://github.com/Microsoft/vscode-languageserver-node/issues/199
            connection.onRequest('client/unregisterFeature', params => this.handleUnregistrationRequest(params));
            connection.onRequest(vscode_languageserver_protocol_1.ApplyWorkspaceEditRequest.type, params => this.handleApplyWorkspaceEdit(params));
            connection.sendNotification(vscode_languageserver_protocol_1.InitializedNotification.type, {});
            this.hookFileEvents(connection);
            this.hookConfigurationChanged(connection);
            this.initializeFeatures(connection);
            this._onReadyCallbacks.resolve();
            return result;
        }).then(undefined, (error) => {
            if (this._clientOptions.initializationFailedHandler) {
                if (this._clientOptions.initializationFailedHandler(error)) {
                    this.initialize(connection);
                }
                else {
                    this.stop();
                    this._onReadyCallbacks.reject(error);
                }
            }
            else if (error instanceof vscode_languageserver_protocol_1.ResponseError && error.data && error.data.retry) {
                vscode_1.window.showErrorMessage(error.message, { title: 'Retry', id: 'retry' }).then(item => {
                    if (item && item.id === 'retry') {
                        this.initialize(connection);
                    }
                    else {
                        this.stop();
                        this._onReadyCallbacks.reject(error);
                    }
                });
            }
            else {
                if (error && error.message) {
                    vscode_1.window.showErrorMessage(error.message);
                }
                this.error('Server initialization failed.', error);
                this.stop();
                this._onReadyCallbacks.reject(error);
            }
            throw error;
        });
    }
    _clientGetRootPath() {
        let folders = vscode_1.workspace.workspaceFolders;
        if (!folders || folders.length === 0) {
            return undefined;
        }
        let folder = folders[0];
        if (folder.uri.scheme === 'file') {
            return folder.uri.fsPath;
        }
        return undefined;
    }
    stop() {
        this._initializeResult = undefined;
        if (!this._connectionPromise) {
            this.state = ClientState.Stopped;
            return Promise.resolve();
        }
        if (this.state === ClientState.Stopping && this._onStop) {
            return this._onStop;
        }
        this.state = ClientState.Stopping;
        this.cleanUp(false);
        // unhook listeners
        return this._onStop = this.resolveConnection().then(connection => {
            return connection.shutdown().then(() => {
                connection.exit();
                connection.end();
                connection.dispose();
                this.state = ClientState.Stopped;
                this.cleanUpChannel();
                this._onStop = undefined;
                this._connectionPromise = undefined;
                this._resolvedConnection = undefined;
            });
        });
    }
    cleanUp(channel = true, diagnostics = true) {
        if (this._listeners) {
            this._listeners.forEach(listener => listener.dispose());
            this._listeners = undefined;
        }
        if (this._providers) {
            this._providers.forEach(provider => provider.dispose());
            this._providers = undefined;
        }
        if (this._syncedDocuments) {
            this._syncedDocuments.clear();
        }
        for (const feature of this._features.values()) {
            feature.dispose();
        }
        if (channel) {
            this.cleanUpChannel();
        }
        if (diagnostics && this._diagnostics) {
            this._diagnostics.dispose();
            this._diagnostics = undefined;
        }
    }
    cleanUpChannel() {
        if (this._outputChannel && this._disposeOutputChannel) {
            this._outputChannel.dispose();
            this._outputChannel = undefined;
        }
    }
    notifyFileEvent(event) {
        var _a;
        const client = this;
        function didChangeWatchedFile(event) {
            client._fileEvents.push(event);
            client._fileEventDelayer.trigger(() => {
                client.onReady().then(() => {
                    client.resolveConnection().then(connection => {
                        if (client.isConnectionActive()) {
                            client.forceDocumentSync();
                            connection.didChangeWatchedFiles({ changes: client._fileEvents });
                        }
                        client._fileEvents = [];
                    });
                }, (error) => {
                    client.error(`Notify file events failed.`, error);
                });
            });
        }
        const workSpaceMiddleware = (_a = this.clientOptions.middleware) === null || _a === void 0 ? void 0 : _a.workspace;
        (workSpaceMiddleware === null || workSpaceMiddleware === void 0 ? void 0 : workSpaceMiddleware.didChangeWatchedFile) ? workSpaceMiddleware.didChangeWatchedFile(event, didChangeWatchedFile) : didChangeWatchedFile(event);
    }
    forceDocumentSync() {
        if (this._didChangeTextDocumentFeature === undefined) {
            this._didChangeTextDocumentFeature = this._dynamicFeatures.get(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type.method);
        }
        this._didChangeTextDocumentFeature.forceDelivery();
    }
    handleDiagnostics(params) {
        if (!this._diagnostics) {
            return;
        }
        let uri = this._p2c.asUri(params.uri);
        let diagnostics = this._p2c.asDiagnostics(params.diagnostics);
        let middleware = this.clientOptions.middleware;
        if (middleware.handleDiagnostics) {
            middleware.handleDiagnostics(uri, diagnostics, (uri, diagnostics) => this.setDiagnostics(uri, diagnostics));
        }
        else {
            this.setDiagnostics(uri, diagnostics);
        }
    }
    setDiagnostics(uri, diagnostics) {
        if (!this._diagnostics) {
            return;
        }
        this._diagnostics.set(uri, diagnostics);
    }
    createConnection() {
        let errorHandler = (error, message, count) => {
            this.handleConnectionError(error, message, count);
        };
        let closeHandler = () => {
            this.handleConnectionClosed();
        };
        return this.createMessageTransports(this._clientOptions.stdioEncoding || 'utf8').then((transports) => {
            return createConnection(transports.reader, transports.writer, errorHandler, closeHandler, this._clientOptions.connectionOptions);
        });
    }
    handleConnectionClosed() {
        // Check whether this is a normal shutdown in progress or the client stopped normally.
        if (this.state === ClientState.Stopping || this.state === ClientState.Stopped) {
            return;
        }
        try {
            if (this._resolvedConnection) {
                this._resolvedConnection.dispose();
            }
        }
        catch (error) {
            // Disposing a connection could fail if error cases.
        }
        let action = CloseAction.DoNotRestart;
        try {
            action = this._clientOptions.errorHandler.closed();
        }
        catch (error) {
            // Ignore errors coming from the error handler.
        }
        this._connectionPromise = undefined;
        this._resolvedConnection = undefined;
        if (action === CloseAction.DoNotRestart) {
            this.error('Connection to server got closed. Server will not be restarted.');
            if (this.state === ClientState.Starting) {
                this._onReadyCallbacks.reject(new Error(`Connection to server got closed. Server will not be restarted.`));
                this.state = ClientState.StartFailed;
            }
            else {
                this.state = ClientState.Stopped;
            }
            this.cleanUp(false, true);
        }
        else if (action === CloseAction.Restart) {
            this.info('Connection to server got closed. Server will restart.');
            this.cleanUp(false, false);
            this.state = ClientState.Initial;
            this.start();
        }
    }
    handleConnectionError(error, message, count) {
        let action = this._clientOptions.errorHandler.error(error, message, count);
        if (action === ErrorAction.Shutdown) {
            this.error('Connection to server is erroring. Shutting down server.');
            this.stop();
        }
    }
    hookConfigurationChanged(connection) {
        vscode_1.workspace.onDidChangeConfiguration(() => {
            this.refreshTrace(connection, true);
        });
    }
    refreshTrace(connection, sendNotification = false) {
        let config = vscode_1.workspace.getConfiguration(this._id);
        let trace = vscode_languageserver_protocol_1.Trace.Off;
        let traceFormat = vscode_languageserver_protocol_1.TraceFormat.Text;
        if (config) {
            const traceConfig = config.get('trace.server', 'off');
            if (typeof traceConfig === 'string') {
                trace = vscode_languageserver_protocol_1.Trace.fromString(traceConfig);
            }
            else {
                trace = vscode_languageserver_protocol_1.Trace.fromString(config.get('trace.server.verbosity', 'off'));
                traceFormat = vscode_languageserver_protocol_1.TraceFormat.fromString(config.get('trace.server.format', 'text'));
            }
        }
        this._trace = trace;
        this._traceFormat = traceFormat;
        connection.trace(this._trace, this._tracer, {
            sendNotification,
            traceFormat: this._traceFormat
        });
    }
    hookFileEvents(_connection) {
        let fileEvents = this._clientOptions.synchronize.fileEvents;
        if (!fileEvents) {
            return;
        }
        let watchers;
        if (Is.array(fileEvents)) {
            watchers = fileEvents;
        }
        else {
            watchers = [fileEvents];
        }
        if (!watchers) {
            return;
        }
        this._dynamicFeatures.get(vscode_languageserver_protocol_1.DidChangeWatchedFilesNotification.type.method).registerRaw(UUID.generateUuid(), watchers);
    }
    registerFeatures(features) {
        for (let feature of features) {
            this.registerFeature(feature);
        }
    }
    registerFeature(feature) {
        this._features.push(feature);
        if (DynamicFeature.is(feature)) {
            const registrationType = feature.registrationType;
            this._dynamicFeatures.set(registrationType.method, feature);
        }
    }
    getFeature(request) {
        return this._dynamicFeatures.get(request);
    }
    registerBuiltinFeatures() {
        this.registerFeature(new ConfigurationFeature(this));
        this.registerFeature(new DidOpenTextDocumentFeature(this, this._syncedDocuments));
        this.registerFeature(new DidChangeTextDocumentFeature(this));
        this.registerFeature(new WillSaveFeature(this));
        this.registerFeature(new WillSaveWaitUntilFeature(this));
        this.registerFeature(new DidSaveTextDocumentFeature(this));
        this.registerFeature(new DidCloseTextDocumentFeature(this, this._syncedDocuments));
        this.registerFeature(new FileSystemWatcherFeature(this, (event) => this.notifyFileEvent(event)));
        this.registerFeature(new CompletionItemFeature(this));
        this.registerFeature(new HoverFeature(this));
        this.registerFeature(new SignatureHelpFeature(this));
        this.registerFeature(new DefinitionFeature(this));
        this.registerFeature(new ReferencesFeature(this));
        this.registerFeature(new DocumentHighlightFeature(this));
        this.registerFeature(new DocumentSymbolFeature(this));
        this.registerFeature(new WorkspaceSymbolFeature(this));
        this.registerFeature(new CodeActionFeature(this));
        this.registerFeature(new CodeLensFeature(this));
        this.registerFeature(new DocumentFormattingFeature(this));
        this.registerFeature(new DocumentRangeFormattingFeature(this));
        this.registerFeature(new DocumentOnTypeFormattingFeature(this));
        this.registerFeature(new RenameFeature(this));
        this.registerFeature(new DocumentLinkFeature(this));
        this.registerFeature(new ExecuteCommandFeature(this));
    }
    fillInitializeParams(params) {
        for (let feature of this._features) {
            if (Is.func(feature.fillInitializeParams)) {
                feature.fillInitializeParams(params);
            }
        }
    }
    computeClientCapabilities() {
        const result = {};
        ensure(result, 'workspace').applyEdit = true;
        const workspaceEdit = ensure(ensure(result, 'workspace'), 'workspaceEdit');
        workspaceEdit.documentChanges = true;
        workspaceEdit.resourceOperations = [vscode_languageserver_protocol_1.ResourceOperationKind.Create, vscode_languageserver_protocol_1.ResourceOperationKind.Rename, vscode_languageserver_protocol_1.ResourceOperationKind.Delete];
        workspaceEdit.failureHandling = vscode_languageserver_protocol_1.FailureHandlingKind.TextOnlyTransactional;
        workspaceEdit.normalizesLineEndings = true;
        workspaceEdit.changeAnnotationSupport = {
            groupsOnLabel: true
        };
        const diagnostics = ensure(ensure(result, 'textDocument'), 'publishDiagnostics');
        diagnostics.relatedInformation = true;
        diagnostics.versionSupport = false;
        diagnostics.tagSupport = { valueSet: [vscode_languageserver_protocol_1.DiagnosticTag.Unnecessary, vscode_languageserver_protocol_1.DiagnosticTag.Deprecated] };
        diagnostics.codeDescriptionSupport = true;
        diagnostics.dataSupport = true;
        const windowCapabilities = ensure(result, 'window');
        const showMessage = ensure(windowCapabilities, 'showMessage');
        showMessage.messageActionItem = { additionalPropertiesSupport: true };
        const showDocument = ensure(windowCapabilities, 'showDocument');
        showDocument.support = true;
        const generalCapabilities = ensure(result, 'general');
        generalCapabilities.regularExpressions = { engine: 'ECMAScript', version: 'ES2020' };
        generalCapabilities.markdown = { parser: 'marked', version: '1.1.0' };
        for (let feature of this._features) {
            feature.fillClientCapabilities(result);
        }
        return result;
    }
    initializeFeatures(_connection) {
        let documentSelector = this._clientOptions.documentSelector;
        for (let feature of this._features) {
            feature.initialize(this._capabilities, documentSelector);
        }
    }
    handleRegistrationRequest(params) {
        return new Promise((resolve, reject) => {
            for (const registration of params.registrations) {
                const feature = this._dynamicFeatures.get(registration.method);
                if (feature === undefined) {
                    reject(new Error(`No feature implementation for ${registration.method} found. Registration failed.`));
                    return;
                }
                const options = registration.registerOptions || {};
                options.documentSelector = options.documentSelector || this._clientOptions.documentSelector;
                const data = {
                    id: registration.id,
                    registerOptions: options
                };
                try {
                    feature.register(data);
                }
                catch (err) {
                    reject(err);
                    return;
                }
            }
            resolve();
        });
    }
    handleUnregistrationRequest(params) {
        return new Promise((resolve, reject) => {
            for (let unregistration of params.unregisterations) {
                const feature = this._dynamicFeatures.get(unregistration.method);
                if (!feature) {
                    reject(new Error(`No feature implementation for ${unregistration.method} found. Unregistration failed.`));
                    return;
                }
                feature.unregister(unregistration.id);
            }
            resolve();
        });
    }
    handleApplyWorkspaceEdit(params) {
        // This is some sort of workaround since the version check should be done by VS Code in the Workspace.applyEdit.
        // However doing it here adds some safety since the server can lag more behind then an extension.
        let workspaceEdit = params.edit;
        let openTextDocuments = new Map();
        vscode_1.workspace.textDocuments.forEach((document) => openTextDocuments.set(document.uri.toString(), document));
        let versionMismatch = false;
        if (workspaceEdit.documentChanges) {
            for (const change of workspaceEdit.documentChanges) {
                if (vscode_languageserver_protocol_1.TextDocumentEdit.is(change) && change.textDocument.version && change.textDocument.version >= 0) {
                    let textDocument = openTextDocuments.get(change.textDocument.uri);
                    if (textDocument && textDocument.version !== change.textDocument.version) {
                        versionMismatch = true;
                        break;
                    }
                }
            }
        }
        if (versionMismatch) {
            return Promise.resolve({ applied: false });
        }
        return Is.asPromise(vscode_1.workspace.applyEdit(this._p2c.asWorkspaceEdit(params.edit)).then((value) => { return { applied: value }; }));
    }
    handleFailedRequest(type, error, defaultValue) {
        // If we get a request cancel or a content modified don't log anything.
        if (error instanceof vscode_languageserver_protocol_1.ResponseError) {
            if (error.code === vscode_languageserver_protocol_1.LSPErrorCodes.RequestCancelled) {
                throw this.makeCancelError();
            }
            else if (error.code === vscode_languageserver_protocol_1.LSPErrorCodes.ContentModified) {
                return defaultValue;
            }
        }
        this.error(`Request ${type.method} failed.`, error);
        throw error;
    }
    makeCancelError() {
        const result = new Error(BaseLanguageClient.Canceled);
        result.name = BaseLanguageClient.Canceled;
        return result;
    }
}
exports.BaseLanguageClient = BaseLanguageClient;
BaseLanguageClient.Canceled = 'Canceled';
//# sourceMappingURL=client.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/codeConverter.js":
/*!***********************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/codeConverter.js ***!
  \***********************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createConverter = void 0;
const code = __webpack_require__(/*! vscode */ "vscode");
const proto = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const Is = __webpack_require__(/*! ./utils/is */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/is.js");
const protocolCompletionItem_1 = __webpack_require__(/*! ./protocolCompletionItem */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCompletionItem.js");
const protocolCodeLens_1 = __webpack_require__(/*! ./protocolCodeLens */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCodeLens.js");
const protocolDocumentLink_1 = __webpack_require__(/*! ./protocolDocumentLink */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolDocumentLink.js");
const protocolCodeAction_1 = __webpack_require__(/*! ./protocolCodeAction */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCodeAction.js");
const protocolDiagnostic_1 = __webpack_require__(/*! ./protocolDiagnostic */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolDiagnostic.js");
const protocolCallHierarchyItem_1 = __webpack_require__(/*! ./protocolCallHierarchyItem */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCallHierarchyItem.js");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
var InsertReplaceRange;
(function (InsertReplaceRange) {
    function is(value) {
        const candidate = value;
        return candidate && !!candidate.inserting && !!candidate.replacing;
    }
    InsertReplaceRange.is = is;
})(InsertReplaceRange || (InsertReplaceRange = {}));
function createConverter(uriConverter) {
    const nullConverter = (value) => value.toString();
    const _uriConverter = uriConverter || nullConverter;
    function asUri(value) {
        return _uriConverter(value);
    }
    function asTextDocumentIdentifier(textDocument) {
        return {
            uri: _uriConverter(textDocument.uri)
        };
    }
    function asVersionedTextDocumentIdentifier(textDocument) {
        return {
            uri: _uriConverter(textDocument.uri),
            version: textDocument.version
        };
    }
    function asOpenTextDocumentParams(textDocument) {
        return {
            textDocument: {
                uri: _uriConverter(textDocument.uri),
                languageId: textDocument.languageId,
                version: textDocument.version,
                text: textDocument.getText()
            }
        };
    }
    function isTextDocumentChangeEvent(value) {
        let candidate = value;
        return !!candidate.document && !!candidate.contentChanges;
    }
    function isTextDocument(value) {
        let candidate = value;
        return !!candidate.uri && !!candidate.version;
    }
    function asChangeTextDocumentParams(arg) {
        if (isTextDocument(arg)) {
            let result = {
                textDocument: {
                    uri: _uriConverter(arg.uri),
                    version: arg.version
                },
                contentChanges: [{ text: arg.getText() }]
            };
            return result;
        }
        else if (isTextDocumentChangeEvent(arg)) {
            let document = arg.document;
            let result = {
                textDocument: {
                    uri: _uriConverter(document.uri),
                    version: document.version
                },
                contentChanges: arg.contentChanges.map((change) => {
                    let range = change.range;
                    return {
                        range: {
                            start: { line: range.start.line, character: range.start.character },
                            end: { line: range.end.line, character: range.end.character }
                        },
                        rangeLength: change.rangeLength,
                        text: change.text
                    };
                })
            };
            return result;
        }
        else {
            throw Error('Unsupported text document change parameter');
        }
    }
    function asCloseTextDocumentParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    function asSaveTextDocumentParams(textDocument, includeContent = false) {
        let result = {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
        if (includeContent) {
            result.text = textDocument.getText();
        }
        return result;
    }
    function asTextDocumentSaveReason(reason) {
        switch (reason) {
            case code.TextDocumentSaveReason.Manual:
                return proto.TextDocumentSaveReason.Manual;
            case code.TextDocumentSaveReason.AfterDelay:
                return proto.TextDocumentSaveReason.AfterDelay;
            case code.TextDocumentSaveReason.FocusOut:
                return proto.TextDocumentSaveReason.FocusOut;
        }
        return proto.TextDocumentSaveReason.Manual;
    }
    function asWillSaveTextDocumentParams(event) {
        return {
            textDocument: asTextDocumentIdentifier(event.document),
            reason: asTextDocumentSaveReason(event.reason)
        };
    }
    function asDidCreateFilesParams(event) {
        return {
            files: event.files.map((fileUri) => ({
                uri: _uriConverter(fileUri),
            })),
        };
    }
    function asDidRenameFilesParams(event) {
        return {
            files: event.files.map((file) => ({
                oldUri: _uriConverter(file.oldUri),
                newUri: _uriConverter(file.newUri),
            })),
        };
    }
    function asDidDeleteFilesParams(event) {
        return {
            files: event.files.map((fileUri) => ({
                uri: _uriConverter(fileUri),
            })),
        };
    }
    function asWillCreateFilesParams(event) {
        return {
            files: event.files.map((fileUri) => ({
                uri: _uriConverter(fileUri),
            })),
        };
    }
    function asWillRenameFilesParams(event) {
        return {
            files: event.files.map((file) => ({
                oldUri: _uriConverter(file.oldUri),
                newUri: _uriConverter(file.newUri),
            })),
        };
    }
    function asWillDeleteFilesParams(event) {
        return {
            files: event.files.map((fileUri) => ({
                uri: _uriConverter(fileUri),
            })),
        };
    }
    function asTextDocumentPositionParams(textDocument, position) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument),
            position: asWorkerPosition(position)
        };
    }
    function asCompletionTriggerKind(triggerKind) {
        switch (triggerKind) {
            case code.CompletionTriggerKind.TriggerCharacter:
                return proto.CompletionTriggerKind.TriggerCharacter;
            case code.CompletionTriggerKind.TriggerForIncompleteCompletions:
                return proto.CompletionTriggerKind.TriggerForIncompleteCompletions;
            default:
                return proto.CompletionTriggerKind.Invoked;
        }
    }
    function asCompletionParams(textDocument, position, context) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument),
            position: asWorkerPosition(position),
            context: {
                triggerKind: asCompletionTriggerKind(context.triggerKind),
                triggerCharacter: context.triggerCharacter
            }
        };
    }
    function asSignatureHelpTriggerKind(triggerKind) {
        switch (triggerKind) {
            case code.SignatureHelpTriggerKind.Invoke:
                return proto.SignatureHelpTriggerKind.Invoked;
            case code.SignatureHelpTriggerKind.TriggerCharacter:
                return proto.SignatureHelpTriggerKind.TriggerCharacter;
            case code.SignatureHelpTriggerKind.ContentChange:
                return proto.SignatureHelpTriggerKind.ContentChange;
        }
    }
    function asParameterInformation(value) {
        // We leave the documentation out on purpose since it usually adds no
        // value for the server.
        return {
            label: value.label
        };
    }
    function asParameterInformations(values) {
        return values.map(asParameterInformation);
    }
    function asSignatureInformation(value) {
        // We leave the documentation out on purpose since it usually adds no
        // value for the server.
        return {
            label: value.label,
            parameters: asParameterInformations(value.parameters)
        };
    }
    function asSignatureInformations(values) {
        return values.map(asSignatureInformation);
    }
    function asSignatureHelp(value) {
        if (value === undefined) {
            return value;
        }
        return {
            signatures: asSignatureInformations(value.signatures),
            activeSignature: value.activeSignature,
            activeParameter: value.activeParameter
        };
    }
    function asSignatureHelpParams(textDocument, position, context) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument),
            position: asWorkerPosition(position),
            context: {
                isRetrigger: context.isRetrigger,
                triggerCharacter: context.triggerCharacter,
                triggerKind: asSignatureHelpTriggerKind(context.triggerKind),
                activeSignatureHelp: asSignatureHelp(context.activeSignatureHelp)
            }
        };
    }
    function asWorkerPosition(position) {
        return { line: position.line, character: position.character };
    }
    function asPosition(value) {
        if (value === undefined || value === null) {
            return value;
        }
        return { line: value.line, character: value.character };
    }
    function asPositions(value) {
        let result = [];
        for (let elem of value) {
            result.push(asPosition(elem));
        }
        return result;
    }
    function asRange(value) {
        if (value === undefined || value === null) {
            return value;
        }
        return { start: asPosition(value.start), end: asPosition(value.end) };
    }
    function asLocation(value) {
        if (value === undefined || value === null) {
            return value;
        }
        return proto.Location.create(asUri(value.uri), asRange(value.range));
    }
    function asDiagnosticSeverity(value) {
        switch (value) {
            case code.DiagnosticSeverity.Error:
                return proto.DiagnosticSeverity.Error;
            case code.DiagnosticSeverity.Warning:
                return proto.DiagnosticSeverity.Warning;
            case code.DiagnosticSeverity.Information:
                return proto.DiagnosticSeverity.Information;
            case code.DiagnosticSeverity.Hint:
                return proto.DiagnosticSeverity.Hint;
        }
    }
    function asDiagnosticTags(tags) {
        if (!tags) {
            return undefined;
        }
        let result = [];
        for (let tag of tags) {
            let converted = asDiagnosticTag(tag);
            if (converted !== undefined) {
                result.push(converted);
            }
        }
        return result.length > 0 ? result : undefined;
    }
    function asDiagnosticTag(tag) {
        switch (tag) {
            case code.DiagnosticTag.Unnecessary:
                return proto.DiagnosticTag.Unnecessary;
            case code.DiagnosticTag.Deprecated:
                return proto.DiagnosticTag.Deprecated;
            default:
                return undefined;
        }
    }
    function asRelatedInformation(item) {
        return {
            message: item.message,
            location: asLocation(item.location)
        };
    }
    function asRelatedInformations(items) {
        return items.map(asRelatedInformation);
    }
    function asDiagnosticCode(value) {
        if (value === undefined || value === null) {
            return undefined;
        }
        if (Is.number(value) || Is.string(value)) {
            return value;
        }
        return { value: value.value, target: asUri(value.target) };
    }
    function asDiagnostic(item) {
        const result = proto.Diagnostic.create(asRange(item.range), item.message);
        const protocolDiagnostic = item instanceof protocolDiagnostic_1.ProtocolDiagnostic ? item : undefined;
        if (protocolDiagnostic !== undefined && protocolDiagnostic.data !== undefined) {
            result.data = protocolDiagnostic.data;
        }
        const code = asDiagnosticCode(item.code);
        if (protocolDiagnostic_1.DiagnosticCode.is(code)) {
            if (protocolDiagnostic !== undefined && protocolDiagnostic.hasDiagnosticCode) {
                result.code = code;
            }
            else {
                result.code = code.value;
                result.codeDescription = { href: code.target };
            }
        }
        else {
            result.code = code;
        }
        if (Is.number(item.severity)) {
            result.severity = asDiagnosticSeverity(item.severity);
        }
        if (Array.isArray(item.tags)) {
            result.tags = asDiagnosticTags(item.tags);
        }
        if (item.relatedInformation) {
            result.relatedInformation = asRelatedInformations(item.relatedInformation);
        }
        if (item.source) {
            result.source = item.source;
        }
        return result;
    }
    function asDiagnostics(items) {
        if (items === undefined || items === null) {
            return items;
        }
        return items.map(asDiagnostic);
    }
    function asDocumentation(format, documentation) {
        switch (format) {
            case '$string':
                return documentation;
            case proto.MarkupKind.PlainText:
                return { kind: format, value: documentation };
            case proto.MarkupKind.Markdown:
                return { kind: format, value: documentation.value };
            default:
                return `Unsupported Markup content received. Kind is: ${format}`;
        }
    }
    function asCompletionItemTag(tag) {
        switch (tag) {
            case code.CompletionItemTag.Deprecated:
                return proto.CompletionItemTag.Deprecated;
        }
        return undefined;
    }
    function asCompletionItemTags(tags) {
        if (tags === undefined) {
            return tags;
        }
        const result = [];
        for (let tag of tags) {
            const converted = asCompletionItemTag(tag);
            if (converted !== undefined) {
                result.push(converted);
            }
        }
        return result;
    }
    function asCompletionItemKind(value, original) {
        if (original !== undefined) {
            return original;
        }
        return value + 1;
    }
    function asCompletionItem(item) {
        let result = { label: item.label };
        let protocolItem = item instanceof protocolCompletionItem_1.default ? item : undefined;
        if (item.detail) {
            result.detail = item.detail;
        }
        // We only send items back we created. So this can't be something else than
        // a string right now.
        if (item.documentation) {
            if (!protocolItem || protocolItem.documentationFormat === '$string') {
                result.documentation = item.documentation;
            }
            else {
                result.documentation = asDocumentation(protocolItem.documentationFormat, item.documentation);
            }
        }
        if (item.filterText) {
            result.filterText = item.filterText;
        }
        fillPrimaryInsertText(result, item);
        if (Is.number(item.kind)) {
            result.kind = asCompletionItemKind(item.kind, protocolItem && protocolItem.originalItemKind);
        }
        if (item.sortText) {
            result.sortText = item.sortText;
        }
        if (item.additionalTextEdits) {
            result.additionalTextEdits = asTextEdits(item.additionalTextEdits);
        }
        if (item.commitCharacters) {
            result.commitCharacters = item.commitCharacters.slice();
        }
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item.preselect === true || item.preselect === false) {
            result.preselect = item.preselect;
        }
        const tags = asCompletionItemTags(item.tags);
        if (protocolItem) {
            if (protocolItem.data !== undefined) {
                result.data = protocolItem.data;
            }
            if (protocolItem.deprecated === true || protocolItem.deprecated === false) {
                if (protocolItem.deprecated === true && tags !== undefined && tags.length > 0) {
                    const index = tags.indexOf(code.CompletionItemTag.Deprecated);
                    if (index !== -1) {
                        tags.splice(index, 1);
                    }
                }
                result.deprecated = protocolItem.deprecated;
            }
            if (protocolItem.insertTextMode !== undefined) {
                result.insertTextMode = protocolItem.insertTextMode;
            }
        }
        if (tags !== undefined && tags.length > 0) {
            result.tags = tags;
        }
        if (result.insertTextMode === undefined && item.keepWhitespace === true) {
            result.insertTextMode = vscode_languageserver_protocol_1.InsertTextMode.adjustIndentation;
        }
        return result;
    }
    function fillPrimaryInsertText(target, source) {
        let format = proto.InsertTextFormat.PlainText;
        let text = undefined;
        let range = undefined;
        if (source.textEdit) {
            text = source.textEdit.newText;
            range = source.textEdit.range;
        }
        else if (source.insertText instanceof code.SnippetString) {
            format = proto.InsertTextFormat.Snippet;
            text = source.insertText.value;
        }
        else {
            text = source.insertText;
        }
        if (source.range) {
            range = source.range;
        }
        target.insertTextFormat = format;
        if (source.fromEdit && text !== undefined && range !== undefined) {
            target.textEdit = asCompletionTextEdit(text, range);
        }
        else {
            target.insertText = text;
        }
    }
    function asCompletionTextEdit(newText, range) {
        if (InsertReplaceRange.is(range)) {
            return proto.InsertReplaceEdit.create(newText, asRange(range.inserting), asRange(range.replacing));
        }
        else {
            return { newText, range: asRange(range) };
        }
    }
    function asTextEdit(edit) {
        return { range: asRange(edit.range), newText: edit.newText };
    }
    function asTextEdits(edits) {
        if (edits === undefined || edits === null) {
            return edits;
        }
        return edits.map(asTextEdit);
    }
    function asSymbolKind(item) {
        if (item <= code.SymbolKind.TypeParameter) {
            // Symbol kind is one based in the protocol and zero based in code.
            return (item + 1);
        }
        return proto.SymbolKind.Property;
    }
    function asSymbolTag(item) {
        return item;
    }
    function asSymbolTags(items) {
        return items.map(asSymbolTag);
    }
    function asReferenceParams(textDocument, position, options) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument),
            position: asWorkerPosition(position),
            context: { includeDeclaration: options.includeDeclaration }
        };
    }
    function asCodeAction(item) {
        let result = proto.CodeAction.create(item.title);
        if (item instanceof protocolCodeAction_1.default && item.data !== undefined) {
            result.data = item.data;
        }
        if (item.kind !== undefined) {
            result.kind = asCodeActionKind(item.kind);
        }
        if (item.diagnostics !== undefined) {
            result.diagnostics = asDiagnostics(item.diagnostics);
        }
        if (item.edit !== undefined) {
            throw new Error(`VS Code code actions can only be converted to a protocol code action without an edit.`);
        }
        if (item.command !== undefined) {
            result.command = asCommand(item.command);
        }
        if (item.isPreferred !== undefined) {
            result.isPreferred = item.isPreferred;
        }
        if (item.disabled !== undefined) {
            result.disabled = { reason: item.disabled.reason };
        }
        return result;
    }
    function asCodeActionContext(context) {
        if (context === undefined || context === null) {
            return context;
        }
        let only;
        if (context.only && Is.string(context.only.value)) {
            only = [context.only.value];
        }
        return proto.CodeActionContext.create(asDiagnostics(context.diagnostics), only);
    }
    function asCodeActionKind(item) {
        if (item === undefined || item === null) {
            return undefined;
        }
        return item.value;
    }
    function asCommand(item) {
        let result = proto.Command.create(item.title, item.command);
        if (item.arguments) {
            result.arguments = item.arguments;
        }
        return result;
    }
    function asCodeLens(item) {
        let result = proto.CodeLens.create(asRange(item.range));
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item instanceof protocolCodeLens_1.default) {
            if (item.data) {
                result.data = item.data;
            }
        }
        return result;
    }
    function asFormattingOptions(options, fileOptions) {
        const result = { tabSize: options.tabSize, insertSpaces: options.insertSpaces };
        if (fileOptions.trimTrailingWhitespace) {
            result.trimTrailingWhitespace = true;
        }
        if (fileOptions.trimFinalNewlines) {
            result.trimFinalNewlines = true;
        }
        if (fileOptions.insertFinalNewline) {
            result.insertFinalNewline = true;
        }
        return result;
    }
    function asDocumentSymbolParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    function asCodeLensParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    function asDocumentLink(item) {
        let result = proto.DocumentLink.create(asRange(item.range));
        if (item.target) {
            result.target = asUri(item.target);
        }
        if (item.tooltip !== undefined) {
            result.tooltip = item.tooltip;
        }
        let protocolItem = item instanceof protocolDocumentLink_1.default ? item : undefined;
        if (protocolItem && protocolItem.data) {
            result.data = protocolItem.data;
        }
        return result;
    }
    function asDocumentLinkParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    function asCallHierarchyItem(value) {
        const result = {
            name: value.name,
            kind: asSymbolKind(value.kind),
            uri: asUri(value.uri),
            range: asRange(value.range),
            selectionRange: asRange(value.selectionRange)
        };
        if (value.detail !== undefined && value.detail.length > 0) {
            result.detail = value.detail;
        }
        if (value.tags !== undefined) {
            result.tags = asSymbolTags(value.tags);
        }
        if (value instanceof protocolCallHierarchyItem_1.default && value.data !== undefined) {
            result.data = value.data;
        }
        return result;
    }
    return {
        asUri,
        asTextDocumentIdentifier,
        asVersionedTextDocumentIdentifier,
        asOpenTextDocumentParams,
        asChangeTextDocumentParams,
        asCloseTextDocumentParams,
        asSaveTextDocumentParams,
        asWillSaveTextDocumentParams,
        asDidCreateFilesParams,
        asDidRenameFilesParams,
        asDidDeleteFilesParams,
        asWillCreateFilesParams,
        asWillRenameFilesParams,
        asWillDeleteFilesParams,
        asTextDocumentPositionParams,
        asCompletionParams,
        asSignatureHelpParams,
        asWorkerPosition,
        asRange,
        asPosition,
        asPositions,
        asLocation,
        asDiagnosticSeverity,
        asDiagnosticTag,
        asDiagnostic,
        asDiagnostics,
        asCompletionItem,
        asTextEdit,
        asSymbolKind,
        asSymbolTag,
        asSymbolTags,
        asReferenceParams,
        asCodeAction,
        asCodeActionContext,
        asCommand,
        asCodeLens,
        asFormattingOptions,
        asDocumentSymbolParams,
        asCodeLensParams,
        asDocumentLink,
        asDocumentLinkParams,
        asCallHierarchyItem
    };
}
exports.createConverter = createConverter;
//# sourceMappingURL=codeConverter.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/colorProvider.js":
/*!***********************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/colorProvider.js ***!
  \***********************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ColorProviderFeature = void 0;
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const client_1 = __webpack_require__(/*! ./client */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js");
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class ColorProviderFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentColorRequest.type);
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'colorProvider').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let [id, options] = this.getRegistration(documentSelector, capabilities.colorProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideColorPresentations: (color, context, token) => {
                const client = this._client;
                const provideColorPresentations = (color, context, token) => {
                    const requestParams = {
                        color,
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(context.document),
                        range: client.code2ProtocolConverter.asRange(context.range)
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.ColorPresentationRequest.type, requestParams, token).then(this.asColorPresentations.bind(this), (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.ColorPresentationRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideColorPresentations
                    ? middleware.provideColorPresentations(color, context, token, provideColorPresentations)
                    : provideColorPresentations(color, context, token);
            },
            provideDocumentColors: (document, token) => {
                const client = this._client;
                const provideDocumentColors = (document, token) => {
                    const requestParams = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document)
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentColorRequest.type, requestParams, token).then(this.asColorInformations.bind(this), (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.ColorPresentationRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideDocumentColors
                    ? middleware.provideDocumentColors(document, token, provideDocumentColors)
                    : provideDocumentColors(document, token);
            }
        };
        return [vscode_1.languages.registerColorProvider(options.documentSelector, provider), provider];
    }
    asColor(color) {
        return new vscode_1.Color(color.red, color.green, color.blue, color.alpha);
    }
    asColorInformations(colorInformation) {
        if (Array.isArray(colorInformation)) {
            return colorInformation.map(ci => {
                return new vscode_1.ColorInformation(this._client.protocol2CodeConverter.asRange(ci.range), this.asColor(ci.color));
            });
        }
        return [];
    }
    asColorPresentations(colorPresentations) {
        if (Array.isArray(colorPresentations)) {
            return colorPresentations.map(cp => {
                let presentation = new vscode_1.ColorPresentation(cp.label);
                presentation.additionalTextEdits = this._client.protocol2CodeConverter.asTextEdits(cp.additionalTextEdits);
                presentation.textEdit = this._client.protocol2CodeConverter.asTextEdit(cp.textEdit);
                return presentation;
            });
        }
        return [];
    }
}
exports.ColorProviderFeature = ColorProviderFeature;
//# sourceMappingURL=colorProvider.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/commonClient.js":
/*!**********************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/commonClient.js ***!
  \**********************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ProposedFeatures = exports.CommonLanguageClient = void 0;
const client_1 = __webpack_require__(/*! ./client */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js");
const colorProvider_1 = __webpack_require__(/*! ./colorProvider */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/colorProvider.js");
const configuration_1 = __webpack_require__(/*! ./configuration */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/configuration.js");
const implementation_1 = __webpack_require__(/*! ./implementation */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/implementation.js");
const typeDefinition_1 = __webpack_require__(/*! ./typeDefinition */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/typeDefinition.js");
const workspaceFolders_1 = __webpack_require__(/*! ./workspaceFolders */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/workspaceFolders.js");
const foldingRange_1 = __webpack_require__(/*! ./foldingRange */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/foldingRange.js");
const declaration_1 = __webpack_require__(/*! ./declaration */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/declaration.js");
const selectionRange_1 = __webpack_require__(/*! ./selectionRange */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/selectionRange.js");
const progress_1 = __webpack_require__(/*! ./progress */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/progress.js");
const callHierarchy_1 = __webpack_require__(/*! ./callHierarchy */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/callHierarchy.js");
const semanticTokens_1 = __webpack_require__(/*! ./semanticTokens */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/semanticTokens.js");
const fileOperations_1 = __webpack_require__(/*! ./fileOperations */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/fileOperations.js");
const linkedEditingRange_1 = __webpack_require__(/*! ./linkedEditingRange */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/linkedEditingRange.js");
class CommonLanguageClient extends client_1.BaseLanguageClient {
    constructor(id, name, clientOptions) {
        super(id, name, clientOptions);
    }
    registerProposedFeatures() {
        this.registerFeatures(ProposedFeatures.createAll(this));
    }
    registerBuiltinFeatures() {
        super.registerBuiltinFeatures();
        this.registerFeature(new configuration_1.ConfigurationFeature(this));
        this.registerFeature(new typeDefinition_1.TypeDefinitionFeature(this));
        this.registerFeature(new implementation_1.ImplementationFeature(this));
        this.registerFeature(new colorProvider_1.ColorProviderFeature(this));
        this.registerFeature(new workspaceFolders_1.WorkspaceFoldersFeature(this));
        this.registerFeature(new foldingRange_1.FoldingRangeFeature(this));
        this.registerFeature(new declaration_1.DeclarationFeature(this));
        this.registerFeature(new selectionRange_1.SelectionRangeFeature(this));
        this.registerFeature(new progress_1.ProgressFeature(this));
        this.registerFeature(new callHierarchy_1.CallHierarchyFeature(this));
        this.registerFeature(new semanticTokens_1.SemanticTokensFeature(this));
        this.registerFeature(new linkedEditingRange_1.LinkedEditingFeature(this));
        this.registerFeature(new fileOperations_1.DidCreateFilesFeature(this));
        this.registerFeature(new fileOperations_1.DidRenameFilesFeature(this));
        this.registerFeature(new fileOperations_1.DidDeleteFilesFeature(this));
        this.registerFeature(new fileOperations_1.WillCreateFilesFeature(this));
        this.registerFeature(new fileOperations_1.WillRenameFilesFeature(this));
        this.registerFeature(new fileOperations_1.WillDeleteFilesFeature(this));
    }
}
exports.CommonLanguageClient = CommonLanguageClient;
// Exporting proposed protocol.
var ProposedFeatures;
(function (ProposedFeatures) {
    function createAll(_client) {
        let result = [];
        return result;
    }
    ProposedFeatures.createAll = createAll;
})(ProposedFeatures = exports.ProposedFeatures || (exports.ProposedFeatures = {}));
//# sourceMappingURL=commonClient.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/configuration.js":
/*!***********************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/configuration.js ***!
  \***********************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.toJSONObject = exports.ConfigurationFeature = void 0;
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
class ConfigurationFeature {
    constructor(_client) {
        this._client = _client;
    }
    fillClientCapabilities(capabilities) {
        capabilities.workspace = capabilities.workspace || {};
        capabilities.workspace.configuration = true;
    }
    initialize() {
        let client = this._client;
        client.onRequest(vscode_languageserver_protocol_1.ConfigurationRequest.type, (params, token) => {
            let configuration = (params) => {
                let result = [];
                for (let item of params.items) {
                    let resource = item.scopeUri !== void 0 && item.scopeUri !== null ? this._client.protocol2CodeConverter.asUri(item.scopeUri) : undefined;
                    result.push(this.getConfiguration(resource, item.section !== null ? item.section : undefined));
                }
                return result;
            };
            let middleware = client.clientOptions.middleware.workspace;
            return middleware && middleware.configuration
                ? middleware.configuration(params, token, configuration)
                : configuration(params, token);
        });
    }
    getConfiguration(resource, section) {
        let result = null;
        if (section) {
            let index = section.lastIndexOf('.');
            if (index === -1) {
                result = toJSONObject(vscode_1.workspace.getConfiguration(undefined, resource).get(section));
            }
            else {
                let config = vscode_1.workspace.getConfiguration(section.substr(0, index), resource);
                if (config) {
                    result = toJSONObject(config.get(section.substr(index + 1)));
                }
            }
        }
        else {
            let config = vscode_1.workspace.getConfiguration(undefined, resource);
            result = {};
            for (let key of Object.keys(config)) {
                if (config.has(key)) {
                    result[key] = toJSONObject(config.get(key));
                }
            }
        }
        if (result === undefined) {
            result = null;
        }
        return result;
    }
    dispose() {
    }
}
exports.ConfigurationFeature = ConfigurationFeature;
function toJSONObject(obj) {
    if (obj) {
        if (Array.isArray(obj)) {
            return obj.map(toJSONObject);
        }
        else if (typeof obj === 'object') {
            const res = Object.create(null);
            for (const key in obj) {
                if (Object.prototype.hasOwnProperty.call(obj, key)) {
                    res[key] = toJSONObject(obj[key]);
                }
            }
            return res;
        }
    }
    return obj;
}
exports.toJSONObject = toJSONObject;
//# sourceMappingURL=configuration.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/declaration.js":
/*!*********************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/declaration.js ***!
  \*********************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DeclarationFeature = void 0;
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const client_1 = __webpack_require__(/*! ./client */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js");
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class DeclarationFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DeclarationRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const declarationSupport = ensure(ensure(capabilities, 'textDocument'), 'declaration');
        declarationSupport.dynamicRegistration = true;
        declarationSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const [id, options] = this.getRegistration(documentSelector, capabilities.declarationProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideDeclaration: (document, position, token) => {
                const client = this._client;
                const provideDeclaration = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.DeclarationRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asDeclarationResult, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DeclarationRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideDeclaration
                    ? middleware.provideDeclaration(document, position, token, provideDeclaration)
                    : provideDeclaration(document, position, token);
            }
        };
        return [vscode_1.languages.registerDeclarationProvider(options.documentSelector, provider), provider];
    }
}
exports.DeclarationFeature = DeclarationFeature;
//# sourceMappingURL=declaration.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/fileOperations.js":
/*!************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/fileOperations.js ***!
  \************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WillDeleteFilesFeature = exports.WillRenameFilesFeature = exports.WillCreateFilesFeature = exports.DidDeleteFilesFeature = exports.DidRenameFilesFeature = exports.DidCreateFilesFeature = void 0;
const code = __webpack_require__(/*! vscode */ "vscode");
const minimatch = __webpack_require__(/*! minimatch */ "./src/lang-server/client/node_modules/minimatch/minimatch.js");
const proto = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const UUID = __webpack_require__(/*! ./utils/uuid */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/uuid.js");
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
function access(target, key) {
    return target[key];
}
function assign(target, key, value) {
    target[key] = value;
}
class FileOperationFeature {
    constructor(client, event, registrationType, clientCapability, serverCapability) {
        this._filters = new Map();
        this._client = client;
        this._event = event;
        this._registrationType = registrationType;
        this._clientCapability = clientCapability;
        this._serverCapability = serverCapability;
    }
    get registrationType() {
        return this._registrationType;
    }
    fillClientCapabilities(capabilities) {
        const value = ensure(ensure(capabilities, 'workspace'), 'fileOperations');
        // this happens n times but it is the same value so we tolerate this.
        assign(value, 'dynamicRegistration', true);
        assign(value, this._clientCapability, true);
    }
    initialize(capabilities) {
        var _a;
        const options = (_a = capabilities.workspace) === null || _a === void 0 ? void 0 : _a.fileOperations;
        const capability = options !== undefined ? access(options, this._serverCapability) : undefined;
        if ((capability === null || capability === void 0 ? void 0 : capability.filters) !== undefined) {
            try {
                this.register({
                    id: UUID.generateUuid(),
                    registerOptions: { filters: capability.filters }
                });
            }
            catch (e) {
                this._client.warn(`Ignoring invalid glob pattern for ${this._serverCapability} registration: ${e}`);
            }
        }
    }
    register(data) {
        if (!this._listener) {
            this._listener = this._event(this.send, this);
        }
        const minimatchFilter = data.registerOptions.filters.map((filter) => {
            const matcher = new minimatch.Minimatch(filter.pattern.glob, FileOperationFeature.asMinimatchOptions(filter.pattern.options));
            if (!matcher.makeRe()) {
                throw new Error(`Invalid pattern ${filter.pattern.glob}!`);
            }
            return { scheme: filter.scheme, matcher, kind: filter.pattern.matches };
        });
        this._filters.set(data.id, minimatchFilter);
    }
    unregister(id) {
        this._filters.delete(id);
        if (this._filters.size === 0 && this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    dispose() {
        this._filters.clear();
        if (this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    async filter(event, prop) {
        // (Asynchronously) map each file onto a boolean of whether it matches
        // any of the globs.
        const fileMatches = await Promise.all(event.files.map(async (item) => {
            const uri = prop(item);
            // Use fsPath to make this consistent with file system watchers but help
            // minimatch to use '/' instead of `\\` if present.
            const path = uri.fsPath.replace(/\\/g, '/');
            for (const filters of this._filters.values()) {
                for (const filter of filters) {
                    if (filter.scheme !== undefined && filter.scheme !== uri.scheme) {
                        continue;
                    }
                    if (filter.matcher.match(path)) {
                        // The pattern matches. If kind is undefined then everything is ok
                        if (filter.kind === undefined) {
                            return true;
                        }
                        const fileType = await FileOperationFeature.getFileType(uri);
                        // If we can't determine the file type than we treat it as a match.
                        // Dropping it would be another alternative.
                        if (fileType === undefined) {
                            this._client.error(`Failed to determine file type for ${uri.toString()}.`);
                            return true;
                        }
                        if ((fileType === code.FileType.File && filter.kind === proto.FileOperationPatternKind.file) || (fileType === code.FileType.Directory && filter.kind === proto.FileOperationPatternKind.folder)) {
                            return true;
                        }
                    }
                    else if (filter.kind === proto.FileOperationPatternKind.folder) {
                        const fileType = await FileOperationFeature.getFileType(uri);
                        if (fileType === code.FileType.Directory && filter.matcher.match(`${path}/`)) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }));
        // Filter the files to those that matched.
        const files = event.files.filter((_, index) => fileMatches[index]);
        return Object.assign(Object.assign({}, event), { files });
    }
    static async getFileType(uri) {
        try {
            return (await code.workspace.fs.stat(uri)).type;
        }
        catch (e) {
            return undefined;
        }
    }
    static asMinimatchOptions(options) {
        if (options === undefined) {
            return undefined;
        }
        if (options.ignoreCase === true) {
            return { nocase: true };
        }
        return undefined;
    }
}
class NotificationFileOperationFeature extends FileOperationFeature {
    constructor(client, event, notificationType, clientCapability, serverCapability, accessUri, createParams) {
        super(client, event, notificationType, clientCapability, serverCapability);
        this._notificationType = notificationType;
        this._accessUri = accessUri;
        this._createParams = createParams;
    }
    async send(originalEvent) {
        // Create a copy of the event that has the files filtered to match what the
        // server wants.
        const filteredEvent = await this.filter(originalEvent, this._accessUri);
        if (filteredEvent.files.length) {
            const next = async (event) => {
                this._client.sendNotification(this._notificationType, this._createParams(event));
            };
            this.doSend(filteredEvent, next);
        }
    }
}
class DidCreateFilesFeature extends NotificationFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onDidCreateFiles, proto.DidCreateFilesNotification.type, 'didCreate', 'didCreate', (i) => i, client.code2ProtocolConverter.asDidCreateFilesParams);
    }
    doSend(event, next) {
        var _a;
        const middleware = (_a = this._client.clientOptions.middleware) === null || _a === void 0 ? void 0 : _a.workspace;
        return (middleware === null || middleware === void 0 ? void 0 : middleware.didCreateFiles) ? middleware.didCreateFiles(event, next)
            : next(event);
    }
}
exports.DidCreateFilesFeature = DidCreateFilesFeature;
class DidRenameFilesFeature extends NotificationFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onDidRenameFiles, proto.DidRenameFilesNotification.type, 'didRename', 'didRename', (i) => i.oldUri, client.code2ProtocolConverter.asDidRenameFilesParams);
    }
    doSend(event, next) {
        var _a;
        const middleware = (_a = this._client.clientOptions.middleware) === null || _a === void 0 ? void 0 : _a.workspace;
        return (middleware === null || middleware === void 0 ? void 0 : middleware.didRenameFiles) ? middleware.didRenameFiles(event, next)
            : next(event);
    }
}
exports.DidRenameFilesFeature = DidRenameFilesFeature;
class DidDeleteFilesFeature extends NotificationFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onDidDeleteFiles, proto.DidDeleteFilesNotification.type, 'didDelete', 'didDelete', (i) => i, client.code2ProtocolConverter.asDidDeleteFilesParams);
    }
    doSend(event, next) {
        var _a;
        const middleware = (_a = this._client.clientOptions.middleware) === null || _a === void 0 ? void 0 : _a.workspace;
        return (middleware === null || middleware === void 0 ? void 0 : middleware.didDeleteFiles) ? middleware.didDeleteFiles(event, next)
            : next(event);
    }
}
exports.DidDeleteFilesFeature = DidDeleteFilesFeature;
class RequestFileOperationFeature extends FileOperationFeature {
    constructor(client, event, requestType, clientCapability, serverCapability, accessUri, createParams) {
        super(client, event, requestType, clientCapability, serverCapability);
        this._requestType = requestType;
        this._accessUri = accessUri;
        this._createParams = createParams;
    }
    async send(originalEvent) {
        const waitUntil = this.waitUntil(originalEvent);
        originalEvent.waitUntil(waitUntil);
    }
    async waitUntil(originalEvent) {
        // Create a copy of the event that has the files filtered to match what the
        // server wants.
        const filteredEvent = await this.filter(originalEvent, this._accessUri);
        if (filteredEvent.files.length) {
            const next = (event) => {
                return this._client.sendRequest(this._requestType, this._createParams(event))
                    .then(this._client.protocol2CodeConverter.asWorkspaceEdit);
            };
            return this.doSend(filteredEvent, next);
        }
        else {
            return undefined;
        }
    }
}
class WillCreateFilesFeature extends RequestFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onWillCreateFiles, proto.WillCreateFilesRequest.type, 'willCreate', 'willCreate', (i) => i, client.code2ProtocolConverter.asWillCreateFilesParams);
    }
    doSend(event, next) {
        var _a;
        const middleware = (_a = this._client.clientOptions.middleware) === null || _a === void 0 ? void 0 : _a.workspace;
        return (middleware === null || middleware === void 0 ? void 0 : middleware.willCreateFiles) ? middleware.willCreateFiles(event, next)
            : next(event);
    }
}
exports.WillCreateFilesFeature = WillCreateFilesFeature;
class WillRenameFilesFeature extends RequestFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onWillRenameFiles, proto.WillRenameFilesRequest.type, 'willRename', 'willRename', (i) => i.oldUri, client.code2ProtocolConverter.asWillRenameFilesParams);
    }
    doSend(event, next) {
        var _a;
        const middleware = (_a = this._client.clientOptions.middleware) === null || _a === void 0 ? void 0 : _a.workspace;
        return (middleware === null || middleware === void 0 ? void 0 : middleware.willRenameFiles) ? middleware.willRenameFiles(event, next)
            : next(event);
    }
}
exports.WillRenameFilesFeature = WillRenameFilesFeature;
class WillDeleteFilesFeature extends RequestFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onWillDeleteFiles, proto.WillDeleteFilesRequest.type, 'willDelete', 'willDelete', (i) => i, client.code2ProtocolConverter.asWillDeleteFilesParams);
    }
    doSend(event, next) {
        var _a;
        const middleware = (_a = this._client.clientOptions.middleware) === null || _a === void 0 ? void 0 : _a.workspace;
        return (middleware === null || middleware === void 0 ? void 0 : middleware.willDeleteFiles) ? middleware.willDeleteFiles(event, next)
            : next(event);
    }
}
exports.WillDeleteFilesFeature = WillDeleteFilesFeature;
//# sourceMappingURL=fileOperations.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/foldingRange.js":
/*!**********************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/foldingRange.js ***!
  \**********************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.FoldingRangeFeature = void 0;
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const client_1 = __webpack_require__(/*! ./client */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js");
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class FoldingRangeFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.FoldingRangeRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let capability = ensure(ensure(capabilities, 'textDocument'), 'foldingRange');
        capability.dynamicRegistration = true;
        capability.rangeLimit = 5000;
        capability.lineFoldingOnly = true;
    }
    initialize(capabilities, documentSelector) {
        let [id, options] = this.getRegistration(documentSelector, capabilities.foldingRangeProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideFoldingRanges: (document, context, token) => {
                const client = this._client;
                const provideFoldingRanges = (document, _, token) => {
                    const requestParams = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document)
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.FoldingRangeRequest.type, requestParams, token).then(FoldingRangeFeature.asFoldingRanges, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.FoldingRangeRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideFoldingRanges
                    ? middleware.provideFoldingRanges(document, context, token, provideFoldingRanges)
                    : provideFoldingRanges(document, context, token);
            }
        };
        return [vscode_1.languages.registerFoldingRangeProvider(options.documentSelector, provider), provider];
    }
    static asFoldingRangeKind(kind) {
        if (kind) {
            switch (kind) {
                case vscode_languageserver_protocol_1.FoldingRangeKind.Comment:
                    return vscode_1.FoldingRangeKind.Comment;
                case vscode_languageserver_protocol_1.FoldingRangeKind.Imports:
                    return vscode_1.FoldingRangeKind.Imports;
                case vscode_languageserver_protocol_1.FoldingRangeKind.Region:
                    return vscode_1.FoldingRangeKind.Region;
            }
        }
        return void 0;
    }
    static asFoldingRanges(foldingRanges) {
        if (Array.isArray(foldingRanges)) {
            return foldingRanges.map(r => {
                return new vscode_1.FoldingRange(r.startLine, r.endLine, FoldingRangeFeature.asFoldingRangeKind(r.kind));
            });
        }
        return [];
    }
}
exports.FoldingRangeFeature = FoldingRangeFeature;
//# sourceMappingURL=foldingRange.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/implementation.js":
/*!************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/implementation.js ***!
  \************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ImplementationFeature = void 0;
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const client_1 = __webpack_require__(/*! ./client */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js");
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class ImplementationFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.ImplementationRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let implementationSupport = ensure(ensure(capabilities, 'textDocument'), 'implementation');
        implementationSupport.dynamicRegistration = true;
        implementationSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        let [id, options] = this.getRegistration(documentSelector, capabilities.implementationProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideImplementation: (document, position, token) => {
                const client = this._client;
                const provideImplementation = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.ImplementationRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asDefinitionResult, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.ImplementationRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideImplementation
                    ? middleware.provideImplementation(document, position, token, provideImplementation)
                    : provideImplementation(document, position, token);
            }
        };
        return [vscode_1.languages.registerImplementationProvider(options.documentSelector, provider), provider];
    }
}
exports.ImplementationFeature = ImplementationFeature;
//# sourceMappingURL=implementation.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/linkedEditingRange.js":
/*!****************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/linkedEditingRange.js ***!
  \****************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
/// <reference path="../../typings/vscode-proposed.d.ts" />
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.LinkedEditingFeature = void 0;
const code = __webpack_require__(/*! vscode */ "vscode");
const proto = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const client_1 = __webpack_require__(/*! ./client */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js");
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class LinkedEditingFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, proto.LinkedEditingRangeRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const linkedEditingSupport = ensure(ensure(capabilities, 'textDocument'), 'linkedEditingRange');
        linkedEditingSupport.dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let [id, options] = this.getRegistration(documentSelector, capabilities.linkedEditingRangeProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideLinkedEditingRanges: (document, position, token) => {
                const client = this._client;
                const provideLinkedEditing = (document, position, token) => {
                    return client.sendRequest(proto.LinkedEditingRangeRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asLinkedEditingRanges, (error) => {
                        return client.handleFailedRequest(proto.LinkedEditingRangeRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideLinkedEditingRange
                    ? middleware.provideLinkedEditingRange(document, position, token, provideLinkedEditing)
                    : provideLinkedEditing(document, position, token);
            }
        };
        return [code.languages.registerLinkedEditingRangeProvider(options.documentSelector, provider), provider];
    }
}
exports.LinkedEditingFeature = LinkedEditingFeature;
//# sourceMappingURL=linkedEditingRange.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/progress.js":
/*!******************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/progress.js ***!
  \******************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ProgressFeature = void 0;
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const progressPart_1 = __webpack_require__(/*! ./progressPart */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/progressPart.js");
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = Object.create(null);
    }
    return target[key];
}
class ProgressFeature {
    constructor(_client) {
        this._client = _client;
        this.activeParts = new Set();
    }
    fillClientCapabilities(capabilities) {
        ensure(capabilities, 'window').workDoneProgress = true;
    }
    initialize() {
        const client = this._client;
        const deleteHandler = (part) => {
            this.activeParts.delete(part);
        };
        const createHandler = (params) => {
            this.activeParts.add(new progressPart_1.ProgressPart(this._client, params.token, deleteHandler));
        };
        client.onRequest(vscode_languageserver_protocol_1.WorkDoneProgressCreateRequest.type, createHandler);
    }
    dispose() {
        for (const part of this.activeParts) {
            part.done();
        }
        this.activeParts.clear();
    }
}
exports.ProgressFeature = ProgressFeature;
//# sourceMappingURL=progress.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/progressPart.js":
/*!**********************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/progressPart.js ***!
  \**********************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ProgressPart = void 0;
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const Is = __webpack_require__(/*! ./utils/is */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/is.js");
class ProgressPart {
    constructor(_client, _token, done) {
        this._client = _client;
        this._token = _token;
        this._reported = 0;
        this._disposable = this._client.onProgress(vscode_languageserver_protocol_1.WorkDoneProgress.type, this._token, (value) => {
            switch (value.kind) {
                case 'begin':
                    this.begin(value);
                    break;
                case 'report':
                    this.report(value);
                    break;
                case 'end':
                    this.done();
                    done && done(this);
                    break;
            }
        });
    }
    begin(params) {
        // Since we don't use commands this will be a silent window progress with a hidden notification.
        vscode_1.window.withProgress({ location: vscode_1.ProgressLocation.Window, cancellable: params.cancellable, title: params.title }, async (progress, cancellationToken) => {
            this._progress = progress;
            this._infinite = params.percentage === undefined;
            this._cancellationToken = cancellationToken;
            this._cancellationToken.onCancellationRequested(() => {
                this._client.sendNotification(vscode_languageserver_protocol_1.WorkDoneProgressCancelNotification.type, { token: this._token });
            });
            this.report(params);
            return new Promise((resolve, reject) => {
                this._resolve = resolve;
                this._reject = reject;
            });
        });
    }
    report(params) {
        if (this._infinite && Is.string(params.message)) {
            this._progress.report({ message: params.message });
        }
        else if (Is.number(params.percentage)) {
            let percentage = Math.max(0, Math.min(params.percentage, 100));
            let delta = Math.max(0, percentage - this._reported);
            this._progress.report({ message: params.message, increment: delta });
            this._reported += delta;
        }
    }
    cancel() {
        if (this._disposable) {
            this._disposable.dispose();
            this._disposable = undefined;
        }
        if (this._reject) {
            this._reject();
            this._resolve = undefined;
            this._reject = undefined;
        }
    }
    done() {
        if (this._disposable) {
            this._disposable.dispose();
            this._disposable = undefined;
        }
        if (this._resolve) {
            this._resolve();
            this._resolve = undefined;
            this._reject = undefined;
        }
    }
}
exports.ProgressPart = ProgressPart;
//# sourceMappingURL=progressPart.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCallHierarchyItem.js":
/*!***********************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCallHierarchyItem.js ***!
  \***********************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(/*! vscode */ "vscode");
class ProtocolCallHierarchyItem extends code.CallHierarchyItem {
    constructor(kind, name, detail, uri, range, selectionRange, data) {
        super(kind, name, detail, uri, range, selectionRange);
        if (data !== undefined) {
            this.data = data;
        }
    }
}
exports.default = ProtocolCallHierarchyItem;
//# sourceMappingURL=protocolCallHierarchyItem.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCodeAction.js":
/*!****************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCodeAction.js ***!
  \****************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode = __webpack_require__(/*! vscode */ "vscode");
class ProtocolCodeAction extends vscode.CodeAction {
    constructor(title, data) {
        super(title);
        this.data = data;
    }
}
exports.default = ProtocolCodeAction;
//# sourceMappingURL=protocolCodeAction.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCodeLens.js":
/*!**************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCodeLens.js ***!
  \**************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(/*! vscode */ "vscode");
class ProtocolCodeLens extends code.CodeLens {
    constructor(range) {
        super(range);
    }
}
exports.default = ProtocolCodeLens;
//# sourceMappingURL=protocolCodeLens.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCompletionItem.js":
/*!********************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCompletionItem.js ***!
  \********************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(/*! vscode */ "vscode");
class ProtocolCompletionItem extends code.CompletionItem {
    constructor(label) {
        super(label);
    }
}
exports.default = ProtocolCompletionItem;
//# sourceMappingURL=protocolCompletionItem.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolConverter.js":
/*!***************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolConverter.js ***!
  \***************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
/// <reference path="../../typings/vscode-proposed.d.ts" />
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createConverter = void 0;
const code = __webpack_require__(/*! vscode */ "vscode");
const ls = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const Is = __webpack_require__(/*! ./utils/is */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/is.js");
const protocolCompletionItem_1 = __webpack_require__(/*! ./protocolCompletionItem */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCompletionItem.js");
const protocolCodeLens_1 = __webpack_require__(/*! ./protocolCodeLens */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCodeLens.js");
const protocolDocumentLink_1 = __webpack_require__(/*! ./protocolDocumentLink */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolDocumentLink.js");
const protocolCodeAction_1 = __webpack_require__(/*! ./protocolCodeAction */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCodeAction.js");
const protocolDiagnostic_1 = __webpack_require__(/*! ./protocolDiagnostic */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolDiagnostic.js");
const protocolCallHierarchyItem_1 = __webpack_require__(/*! ./protocolCallHierarchyItem */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolCallHierarchyItem.js");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
var CodeBlock;
(function (CodeBlock) {
    function is(value) {
        let candidate = value;
        return candidate && Is.string(candidate.language) && Is.string(candidate.value);
    }
    CodeBlock.is = is;
})(CodeBlock || (CodeBlock = {}));
function createConverter(uriConverter, trustMarkdown) {
    const nullConverter = (value) => code.Uri.parse(value);
    const _uriConverter = uriConverter || nullConverter;
    function asUri(value) {
        return _uriConverter(value);
    }
    function asDiagnostics(diagnostics) {
        return diagnostics.map(asDiagnostic);
    }
    function asDiagnostic(diagnostic) {
        let result = new protocolDiagnostic_1.ProtocolDiagnostic(asRange(diagnostic.range), diagnostic.message, asDiagnosticSeverity(diagnostic.severity), diagnostic.data);
        if (diagnostic.code !== undefined) {
            if (ls.CodeDescription.is(diagnostic.codeDescription)) {
                result.code = {
                    value: diagnostic.code,
                    target: asUri(diagnostic.codeDescription.href)
                };
            }
            else if (protocolDiagnostic_1.DiagnosticCode.is(diagnostic.code)) {
                result.hasDiagnosticCode = true;
                result.code = {
                    value: diagnostic.code.value,
                    target: asUri(diagnostic.code.target)
                };
            }
            else {
                result.code = diagnostic.code;
            }
        }
        if (diagnostic.source) {
            result.source = diagnostic.source;
        }
        if (diagnostic.relatedInformation) {
            result.relatedInformation = asRelatedInformation(diagnostic.relatedInformation);
        }
        if (Array.isArray(diagnostic.tags)) {
            result.tags = asDiagnosticTags(diagnostic.tags);
        }
        return result;
    }
    function asRelatedInformation(relatedInformation) {
        return relatedInformation.map(asDiagnosticRelatedInformation);
    }
    function asDiagnosticRelatedInformation(information) {
        return new code.DiagnosticRelatedInformation(asLocation(information.location), information.message);
    }
    function asDiagnosticTags(tags) {
        if (!tags) {
            return undefined;
        }
        let result = [];
        for (let tag of tags) {
            let converted = asDiagnosticTag(tag);
            if (converted !== undefined) {
                result.push(converted);
            }
        }
        return result.length > 0 ? result : undefined;
    }
    function asDiagnosticTag(tag) {
        switch (tag) {
            case ls.DiagnosticTag.Unnecessary:
                return code.DiagnosticTag.Unnecessary;
            case ls.DiagnosticTag.Deprecated:
                return code.DiagnosticTag.Deprecated;
            default:
                return undefined;
        }
    }
    function asPosition(value) {
        if (!value) {
            return undefined;
        }
        return new code.Position(value.line, value.character);
    }
    function asRange(value) {
        if (!value) {
            return undefined;
        }
        return new code.Range(asPosition(value.start), asPosition(value.end));
    }
    function asRanges(value) {
        return value.map(value => asRange(value));
    }
    function asDiagnosticSeverity(value) {
        if (value === undefined || value === null) {
            return code.DiagnosticSeverity.Error;
        }
        switch (value) {
            case ls.DiagnosticSeverity.Error:
                return code.DiagnosticSeverity.Error;
            case ls.DiagnosticSeverity.Warning:
                return code.DiagnosticSeverity.Warning;
            case ls.DiagnosticSeverity.Information:
                return code.DiagnosticSeverity.Information;
            case ls.DiagnosticSeverity.Hint:
                return code.DiagnosticSeverity.Hint;
        }
        return code.DiagnosticSeverity.Error;
    }
    function asHoverContent(value) {
        if (Is.string(value)) {
            return asMarkdownString(value);
        }
        else if (CodeBlock.is(value)) {
            let result = asMarkdownString();
            return result.appendCodeblock(value.value, value.language);
        }
        else if (Array.isArray(value)) {
            let result = [];
            for (let element of value) {
                let item = asMarkdownString();
                if (CodeBlock.is(element)) {
                    item.appendCodeblock(element.value, element.language);
                }
                else {
                    item.appendMarkdown(element);
                }
                result.push(item);
            }
            return result;
        }
        else {
            let result;
            switch (value.kind) {
                case ls.MarkupKind.Markdown:
                    return asMarkdownString(value.value);
                case ls.MarkupKind.PlainText:
                    result = asMarkdownString();
                    result.appendText(value.value);
                    return result;
                default:
                    result = asMarkdownString();
                    result.appendText(`Unsupported Markup content received. Kind is: ${value.kind}`);
                    return result;
            }
        }
    }
    function asDocumentation(value) {
        if (Is.string(value)) {
            return value;
        }
        else {
            switch (value.kind) {
                case ls.MarkupKind.Markdown:
                    return asMarkdownString(value.value);
                case ls.MarkupKind.PlainText:
                    return value.value;
                default:
                    return `Unsupported Markup content received. Kind is: ${value.kind}`;
            }
        }
    }
    function asMarkdownString(value) {
        const result = new code.MarkdownString(value);
        if (trustMarkdown === true) {
            result.isTrusted = trustMarkdown;
        }
        return result;
    }
    function asHover(hover) {
        if (!hover) {
            return undefined;
        }
        return new code.Hover(asHoverContent(hover.contents), asRange(hover.range));
    }
    function asCompletionResult(result) {
        if (!result) {
            return undefined;
        }
        if (Array.isArray(result)) {
            let items = result;
            return items.map(asCompletionItem);
        }
        let list = result;
        return new code.CompletionList(list.items.map(asCompletionItem), list.isIncomplete);
    }
    function asCompletionItemKind(value) {
        // Protocol item kind is 1 based, codes item kind is zero based.
        if (ls.CompletionItemKind.Text <= value && value <= ls.CompletionItemKind.TypeParameter) {
            return [value - 1, undefined];
        }
        return [code.CompletionItemKind.Text, value];
    }
    function asCompletionItemTag(tag) {
        switch (tag) {
            case ls.CompletionItemTag.Deprecated:
                return code.CompletionItemTag.Deprecated;
        }
        return undefined;
    }
    function asCompletionItemTags(tags) {
        if (tags === undefined || tags === null) {
            return [];
        }
        const result = [];
        for (let tag of tags) {
            const converted = asCompletionItemTag(tag);
            if (converted !== undefined) {
                result.push(converted);
            }
        }
        return result;
    }
    function asCompletionItem(item) {
        let tags = asCompletionItemTags(item.tags);
        let result = new protocolCompletionItem_1.default(item.label);
        if (item.detail) {
            result.detail = item.detail;
        }
        if (item.documentation) {
            result.documentation = asDocumentation(item.documentation);
            result.documentationFormat = Is.string(item.documentation) ? '$string' : item.documentation.kind;
        }
        if (item.filterText) {
            result.filterText = item.filterText;
        }
        let insertText = asCompletionInsertText(item);
        if (insertText) {
            result.insertText = insertText.text;
            result.range = insertText.range;
            result.fromEdit = insertText.fromEdit;
        }
        if (Is.number(item.kind)) {
            let [itemKind, original] = asCompletionItemKind(item.kind);
            result.kind = itemKind;
            if (original) {
                result.originalItemKind = original;
            }
        }
        if (item.sortText) {
            result.sortText = item.sortText;
        }
        if (item.additionalTextEdits) {
            result.additionalTextEdits = asTextEdits(item.additionalTextEdits);
        }
        if (Is.stringArray(item.commitCharacters)) {
            result.commitCharacters = item.commitCharacters.slice();
        }
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item.deprecated === true || item.deprecated === false) {
            result.deprecated = item.deprecated;
            if (item.deprecated === true) {
                tags.push(code.CompletionItemTag.Deprecated);
            }
        }
        if (item.preselect === true || item.preselect === false) {
            result.preselect = item.preselect;
        }
        if (item.data !== undefined) {
            result.data = item.data;
        }
        if (tags.length > 0) {
            result.tags = tags;
        }
        if (item.insertTextMode !== undefined) {
            result.insertTextMode = item.insertTextMode;
            if (item.insertTextMode === vscode_languageserver_protocol_1.InsertTextMode.asIs) {
                result.keepWhitespace = true;
            }
        }
        return result;
    }
    function asCompletionInsertText(item) {
        if (item.textEdit) {
            if (item.insertTextFormat === ls.InsertTextFormat.Snippet) {
                return { text: new code.SnippetString(item.textEdit.newText), range: asCompletionRange(item.textEdit), fromEdit: true };
            }
            else {
                return { text: item.textEdit.newText, range: asCompletionRange(item.textEdit), fromEdit: true };
            }
        }
        else if (item.insertText) {
            if (item.insertTextFormat === ls.InsertTextFormat.Snippet) {
                return { text: new code.SnippetString(item.insertText), fromEdit: false };
            }
            else {
                return { text: item.insertText, fromEdit: false };
            }
        }
        else {
            return undefined;
        }
    }
    function asCompletionRange(value) {
        if (ls.InsertReplaceEdit.is(value)) {
            return { inserting: asRange(value.insert), replacing: asRange(value.replace) };
        }
        else {
            return asRange(value.range);
        }
    }
    function asTextEdit(edit) {
        if (!edit) {
            return undefined;
        }
        return new code.TextEdit(asRange(edit.range), edit.newText);
    }
    function asTextEdits(items) {
        if (!items) {
            return undefined;
        }
        return items.map(asTextEdit);
    }
    function asSignatureHelp(item) {
        if (!item) {
            return undefined;
        }
        let result = new code.SignatureHelp();
        if (Is.number(item.activeSignature)) {
            result.activeSignature = item.activeSignature;
        }
        else {
            // activeSignature was optional in the past
            result.activeSignature = 0;
        }
        if (Is.number(item.activeParameter)) {
            result.activeParameter = item.activeParameter;
        }
        else {
            // activeParameter was optional in the past
            result.activeParameter = 0;
        }
        if (item.signatures) {
            result.signatures = asSignatureInformations(item.signatures);
        }
        return result;
    }
    function asSignatureInformations(items) {
        return items.map(asSignatureInformation);
    }
    function asSignatureInformation(item) {
        let result = new code.SignatureInformation(item.label);
        if (item.documentation !== undefined) {
            result.documentation = asDocumentation(item.documentation);
        }
        if (item.parameters !== undefined) {
            result.parameters = asParameterInformations(item.parameters);
        }
        if (item.activeParameter !== undefined) {
            result.activeParameter = item.activeParameter;
        }
        {
            return result;
        }
    }
    function asParameterInformations(item) {
        return item.map(asParameterInformation);
    }
    function asParameterInformation(item) {
        let result = new code.ParameterInformation(item.label);
        if (item.documentation) {
            result.documentation = asDocumentation(item.documentation);
        }
        return result;
    }
    function asLocation(item) {
        if (!item) {
            return undefined;
        }
        return new code.Location(_uriConverter(item.uri), asRange(item.range));
    }
    function asDeclarationResult(item) {
        if (!item) {
            return undefined;
        }
        return asLocationResult(item);
    }
    function asDefinitionResult(item) {
        if (!item) {
            return undefined;
        }
        return asLocationResult(item);
    }
    function asLocationLink(item) {
        if (!item) {
            return undefined;
        }
        let result = {
            targetUri: _uriConverter(item.targetUri),
            targetRange: asRange(item.targetRange),
            originSelectionRange: asRange(item.originSelectionRange),
            targetSelectionRange: asRange(item.targetSelectionRange)
        };
        if (!result.targetSelectionRange) {
            throw new Error(`targetSelectionRange must not be undefined or null`);
        }
        return result;
    }
    function asLocationResult(item) {
        if (!item) {
            return undefined;
        }
        if (Is.array(item)) {
            if (item.length === 0) {
                return [];
            }
            else if (ls.LocationLink.is(item[0])) {
                let links = item;
                return links.map((link) => asLocationLink(link));
            }
            else {
                let locations = item;
                return locations.map((location) => asLocation(location));
            }
        }
        else if (ls.LocationLink.is(item)) {
            return [asLocationLink(item)];
        }
        else {
            return asLocation(item);
        }
    }
    function asReferences(values) {
        if (!values) {
            return undefined;
        }
        return values.map(location => asLocation(location));
    }
    function asDocumentHighlights(values) {
        if (!values) {
            return undefined;
        }
        return values.map(asDocumentHighlight);
    }
    function asDocumentHighlight(item) {
        let result = new code.DocumentHighlight(asRange(item.range));
        if (Is.number(item.kind)) {
            result.kind = asDocumentHighlightKind(item.kind);
        }
        return result;
    }
    function asDocumentHighlightKind(item) {
        switch (item) {
            case ls.DocumentHighlightKind.Text:
                return code.DocumentHighlightKind.Text;
            case ls.DocumentHighlightKind.Read:
                return code.DocumentHighlightKind.Read;
            case ls.DocumentHighlightKind.Write:
                return code.DocumentHighlightKind.Write;
        }
        return code.DocumentHighlightKind.Text;
    }
    function asSymbolInformations(values, uri) {
        if (!values) {
            return undefined;
        }
        return values.map(information => asSymbolInformation(information, uri));
    }
    function asSymbolKind(item) {
        if (item <= ls.SymbolKind.TypeParameter) {
            // Symbol kind is one based in the protocol and zero based in code.
            return item - 1;
        }
        return code.SymbolKind.Property;
    }
    function asSymbolTag(value) {
        switch (value) {
            case ls.SymbolTag.Deprecated:
                return code.SymbolTag.Deprecated;
            default:
                return undefined;
        }
    }
    function asSymbolTags(items) {
        if (items === undefined || items === null) {
            return undefined;
        }
        const result = [];
        for (const item of items) {
            const converted = asSymbolTag(item);
            if (converted !== undefined) {
                result.push(converted);
            }
        }
        return result.length === 0 ? undefined : result;
    }
    function asSymbolInformation(item, uri) {
        // Symbol kind is one based in the protocol and zero based in code.
        let result = new code.SymbolInformation(item.name, asSymbolKind(item.kind), asRange(item.location.range), item.location.uri ? _uriConverter(item.location.uri) : uri);
        fillTags(result, item);
        if (item.containerName) {
            result.containerName = item.containerName;
        }
        return result;
    }
    function asDocumentSymbols(values) {
        if (values === undefined || values === null) {
            return undefined;
        }
        return values.map(asDocumentSymbol);
    }
    function asDocumentSymbol(value) {
        let result = new code.DocumentSymbol(value.name, value.detail || '', asSymbolKind(value.kind), asRange(value.range), asRange(value.selectionRange));
        fillTags(result, value);
        if (value.children !== undefined && value.children.length > 0) {
            let children = [];
            for (let child of value.children) {
                children.push(asDocumentSymbol(child));
            }
            result.children = children;
        }
        return result;
    }
    function fillTags(result, value) {
        result.tags = asSymbolTags(value.tags);
        if (value.deprecated) {
            if (!result.tags) {
                result.tags = [code.SymbolTag.Deprecated];
            }
            else {
                if (!result.tags.includes(code.SymbolTag.Deprecated)) {
                    result.tags = result.tags.concat(code.SymbolTag.Deprecated);
                }
            }
        }
    }
    function asCommand(item) {
        let result = { title: item.title, command: item.command };
        if (item.arguments) {
            result.arguments = item.arguments;
        }
        return result;
    }
    function asCommands(items) {
        if (!items) {
            return undefined;
        }
        return items.map(asCommand);
    }
    const kindMapping = new Map();
    kindMapping.set(ls.CodeActionKind.Empty, code.CodeActionKind.Empty);
    kindMapping.set(ls.CodeActionKind.QuickFix, code.CodeActionKind.QuickFix);
    kindMapping.set(ls.CodeActionKind.Refactor, code.CodeActionKind.Refactor);
    kindMapping.set(ls.CodeActionKind.RefactorExtract, code.CodeActionKind.RefactorExtract);
    kindMapping.set(ls.CodeActionKind.RefactorInline, code.CodeActionKind.RefactorInline);
    kindMapping.set(ls.CodeActionKind.RefactorRewrite, code.CodeActionKind.RefactorRewrite);
    kindMapping.set(ls.CodeActionKind.Source, code.CodeActionKind.Source);
    kindMapping.set(ls.CodeActionKind.SourceOrganizeImports, code.CodeActionKind.SourceOrganizeImports);
    function asCodeActionKind(item) {
        if (item === undefined || item === null) {
            return undefined;
        }
        let result = kindMapping.get(item);
        if (result) {
            return result;
        }
        let parts = item.split('.');
        result = code.CodeActionKind.Empty;
        for (let part of parts) {
            result = result.append(part);
        }
        return result;
    }
    function asCodeActionKinds(items) {
        if (items === undefined || items === null) {
            return undefined;
        }
        return items.map(kind => asCodeActionKind(kind));
    }
    function asCodeAction(item) {
        if (item === undefined || item === null) {
            return undefined;
        }
        let result = new protocolCodeAction_1.default(item.title, item.data);
        if (item.kind !== undefined) {
            result.kind = asCodeActionKind(item.kind);
        }
        if (item.diagnostics !== undefined) {
            result.diagnostics = asDiagnostics(item.diagnostics);
        }
        if (item.edit !== undefined) {
            result.edit = asWorkspaceEdit(item.edit);
        }
        if (item.command !== undefined) {
            result.command = asCommand(item.command);
        }
        if (item.isPreferred !== undefined) {
            result.isPreferred = item.isPreferred;
        }
        if (item.disabled !== undefined) {
            result.disabled = { reason: item.disabled.reason };
        }
        return result;
    }
    function asCodeLens(item) {
        if (!item) {
            return undefined;
        }
        let result = new protocolCodeLens_1.default(asRange(item.range));
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item.data !== undefined && item.data !== null) {
            result.data = item.data;
        }
        return result;
    }
    function asCodeLenses(items) {
        if (!items) {
            return undefined;
        }
        return items.map((codeLens) => asCodeLens(codeLens));
    }
    function asWorkspaceEdit(item) {
        if (!item) {
            return undefined;
        }
        const sharedMetadata = new Map();
        if (item.changeAnnotations !== undefined) {
            for (const key of Object.keys(item.changeAnnotations)) {
                const metaData = asWorkspaceEditEntryMetadata(item.changeAnnotations[key]);
                sharedMetadata.set(key, metaData);
            }
        }
        const asMetadata = (annotation) => {
            if (annotation === undefined) {
                return undefined;
            }
            else {
                return sharedMetadata.get(annotation);
            }
        };
        const result = new code.WorkspaceEdit();
        if (item.documentChanges) {
            for (const change of item.documentChanges) {
                if (ls.CreateFile.is(change)) {
                    result.createFile(_uriConverter(change.uri), change.options, asMetadata(change.annotationId));
                }
                else if (ls.RenameFile.is(change)) {
                    result.renameFile(_uriConverter(change.oldUri), _uriConverter(change.newUri), change.options, asMetadata(change.annotationId));
                }
                else if (ls.DeleteFile.is(change)) {
                    result.deleteFile(_uriConverter(change.uri), change.options, asMetadata(change.annotationId));
                }
                else if (ls.TextDocumentEdit.is(change)) {
                    const uri = _uriConverter(change.textDocument.uri);
                    for (const edit of change.edits) {
                        if (vscode_languageserver_protocol_1.AnnotatedTextEdit.is(edit)) {
                            result.replace(uri, asRange(edit.range), edit.newText, asMetadata(edit.annotationId));
                        }
                        else {
                            result.replace(uri, asRange(edit.range), edit.newText);
                        }
                    }
                }
                else {
                    throw new Error(`Unknown workspace edit change received:\n${JSON.stringify(change, undefined, 4)}`);
                }
            }
        }
        else if (item.changes) {
            Object.keys(item.changes).forEach(key => {
                result.set(_uriConverter(key), asTextEdits(item.changes[key]));
            });
        }
        return result;
    }
    function asWorkspaceEditEntryMetadata(annotation) {
        if (annotation === undefined) {
            return undefined;
        }
        return { label: annotation.label, needsConfirmation: !!annotation.needsConfirmation, description: annotation.description };
    }
    function asDocumentLink(item) {
        let range = asRange(item.range);
        let target = item.target ? asUri(item.target) : undefined;
        // target must be optional in DocumentLink
        let link = new protocolDocumentLink_1.default(range, target);
        if (item.tooltip !== undefined) {
            link.tooltip = item.tooltip;
        }
        if (item.data !== undefined && item.data !== null) {
            link.data = item.data;
        }
        return link;
    }
    function asDocumentLinks(items) {
        if (!items) {
            return undefined;
        }
        return items.map(asDocumentLink);
    }
    function asColor(color) {
        return new code.Color(color.red, color.green, color.blue, color.alpha);
    }
    function asColorInformation(ci) {
        return new code.ColorInformation(asRange(ci.range), asColor(ci.color));
    }
    function asColorInformations(colorInformation) {
        if (Array.isArray(colorInformation)) {
            return colorInformation.map(asColorInformation);
        }
        return undefined;
    }
    function asColorPresentation(cp) {
        let presentation = new code.ColorPresentation(cp.label);
        presentation.additionalTextEdits = asTextEdits(cp.additionalTextEdits);
        if (cp.textEdit) {
            presentation.textEdit = asTextEdit(cp.textEdit);
        }
        return presentation;
    }
    function asColorPresentations(colorPresentations) {
        if (Array.isArray(colorPresentations)) {
            return colorPresentations.map(asColorPresentation);
        }
        return undefined;
    }
    function asFoldingRangeKind(kind) {
        if (kind) {
            switch (kind) {
                case ls.FoldingRangeKind.Comment:
                    return code.FoldingRangeKind.Comment;
                case ls.FoldingRangeKind.Imports:
                    return code.FoldingRangeKind.Imports;
                case ls.FoldingRangeKind.Region:
                    return code.FoldingRangeKind.Region;
            }
        }
        return undefined;
    }
    function asFoldingRange(r) {
        return new code.FoldingRange(r.startLine, r.endLine, asFoldingRangeKind(r.kind));
    }
    function asFoldingRanges(foldingRanges) {
        if (Array.isArray(foldingRanges)) {
            return foldingRanges.map(asFoldingRange);
        }
        return undefined;
    }
    function asSelectionRange(selectionRange) {
        return new code.SelectionRange(asRange(selectionRange.range), selectionRange.parent ? asSelectionRange(selectionRange.parent) : undefined);
    }
    function asSelectionRanges(selectionRanges) {
        if (!Array.isArray(selectionRanges)) {
            return [];
        }
        let result = [];
        for (let range of selectionRanges) {
            result.push(asSelectionRange(range));
        }
        return result;
    }
    function asCallHierarchyItem(item) {
        if (item === null) {
            return undefined;
        }
        let result = new protocolCallHierarchyItem_1.default(asSymbolKind(item.kind), item.name, item.detail || '', asUri(item.uri), asRange(item.range), asRange(item.selectionRange), item.data);
        if (item.tags !== undefined) {
            result.tags = asSymbolTags(item.tags);
        }
        return result;
    }
    function asCallHierarchyItems(items) {
        if (items === null) {
            return undefined;
        }
        return items.map(item => asCallHierarchyItem(item));
    }
    function asCallHierarchyIncomingCall(item) {
        return new code.CallHierarchyIncomingCall(asCallHierarchyItem(item.from), asRanges(item.fromRanges));
    }
    function asCallHierarchyIncomingCalls(items) {
        if (items === null) {
            return undefined;
        }
        return items.map(item => asCallHierarchyIncomingCall(item));
    }
    function asCallHierarchyOutgoingCall(item) {
        return new code.CallHierarchyOutgoingCall(asCallHierarchyItem(item.to), asRanges(item.fromRanges));
    }
    function asCallHierarchyOutgoingCalls(items) {
        if (items === null) {
            return undefined;
        }
        return items.map(item => asCallHierarchyOutgoingCall(item));
    }
    function asSemanticTokens(value) {
        if (value === undefined || value === null) {
            return undefined;
        }
        return new code.SemanticTokens(new Uint32Array(value.data), value.resultId);
    }
    function asSemanticTokensEdit(value) {
        return new code.SemanticTokensEdit(value.start, value.deleteCount, value.data !== undefined ? new Uint32Array(value.data) : undefined);
    }
    function asSemanticTokensEdits(value) {
        if (value === undefined || value === null) {
            return undefined;
        }
        return new code.SemanticTokensEdits(value.edits.map(asSemanticTokensEdit), value.resultId);
    }
    function asSemanticTokensLegend(value) {
        return value;
    }
    function asLinkedEditingRanges(value) {
        if (value === null || value === undefined) {
            return undefined;
        }
        return new code.LinkedEditingRanges(asRanges(value.ranges), asRegularExpression(value.wordPattern));
    }
    function asRegularExpression(value) {
        if (value === null || value === undefined) {
            return undefined;
        }
        return new RegExp(value);
    }
    return {
        asUri,
        asDiagnostics,
        asDiagnostic,
        asRange,
        asRanges,
        asPosition,
        asDiagnosticSeverity,
        asDiagnosticTag,
        asHover,
        asCompletionResult,
        asCompletionItem,
        asTextEdit,
        asTextEdits,
        asSignatureHelp,
        asSignatureInformations,
        asSignatureInformation,
        asParameterInformations,
        asParameterInformation,
        asDeclarationResult,
        asDefinitionResult,
        asLocation,
        asReferences,
        asDocumentHighlights,
        asDocumentHighlight,
        asDocumentHighlightKind,
        asSymbolKind,
        asSymbolTag,
        asSymbolTags,
        asSymbolInformations,
        asSymbolInformation,
        asDocumentSymbols,
        asDocumentSymbol,
        asCommand,
        asCommands,
        asCodeAction,
        asCodeActionKind,
        asCodeActionKinds,
        asCodeLens,
        asCodeLenses,
        asWorkspaceEdit,
        asDocumentLink,
        asDocumentLinks,
        asFoldingRangeKind,
        asFoldingRange,
        asFoldingRanges,
        asColor,
        asColorInformation,
        asColorInformations,
        asColorPresentation,
        asColorPresentations,
        asSelectionRange,
        asSelectionRanges,
        asSemanticTokensLegend,
        asSemanticTokens,
        asSemanticTokensEdit,
        asSemanticTokensEdits,
        asCallHierarchyItem,
        asCallHierarchyItems,
        asCallHierarchyIncomingCall,
        asCallHierarchyIncomingCalls,
        asCallHierarchyOutgoingCall,
        asCallHierarchyOutgoingCalls,
        asLinkedEditingRanges: asLinkedEditingRanges
    };
}
exports.createConverter = createConverter;
//# sourceMappingURL=protocolConverter.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolDiagnostic.js":
/*!****************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolDiagnostic.js ***!
  \****************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ProtocolDiagnostic = exports.DiagnosticCode = void 0;
const vscode = __webpack_require__(/*! vscode */ "vscode");
const Is = __webpack_require__(/*! ./utils/is */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/is.js");
var DiagnosticCode;
(function (DiagnosticCode) {
    function is(value) {
        const candidate = value;
        return candidate !== undefined && candidate !== null && (Is.number(candidate.value) || Is.string(candidate.value)) && Is.string(candidate.target);
    }
    DiagnosticCode.is = is;
})(DiagnosticCode = exports.DiagnosticCode || (exports.DiagnosticCode = {}));
class ProtocolDiagnostic extends vscode.Diagnostic {
    constructor(range, message, severity, data) {
        super(range, message, severity);
        this.data = data;
        this.hasDiagnosticCode = false;
    }
}
exports.ProtocolDiagnostic = ProtocolDiagnostic;
//# sourceMappingURL=protocolDiagnostic.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolDocumentLink.js":
/*!******************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/protocolDocumentLink.js ***!
  \******************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(/*! vscode */ "vscode");
class ProtocolDocumentLink extends code.DocumentLink {
    constructor(range, target) {
        super(range, target);
    }
}
exports.default = ProtocolDocumentLink;
//# sourceMappingURL=protocolDocumentLink.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/selectionRange.js":
/*!************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/selectionRange.js ***!
  \************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SelectionRangeFeature = void 0;
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const client_1 = __webpack_require__(/*! ./client */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js");
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = Object.create(null);
    }
    return target[key];
}
class SelectionRangeFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.SelectionRangeRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let capability = ensure(ensure(capabilities, 'textDocument'), 'selectionRange');
        capability.dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let [id, options] = this.getRegistration(documentSelector, capabilities.selectionRangeProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideSelectionRanges: (document, positions, token) => {
                const client = this._client;
                const provideSelectionRanges = (document, positions, token) => {
                    const requestParams = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        positions: client.code2ProtocolConverter.asPositions(positions)
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.SelectionRangeRequest.type, requestParams, token).then((ranges) => client.protocol2CodeConverter.asSelectionRanges(ranges), (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.SelectionRangeRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideSelectionRanges
                    ? middleware.provideSelectionRanges(document, positions, token, provideSelectionRanges)
                    : provideSelectionRanges(document, positions, token);
            }
        };
        return [vscode_1.languages.registerSelectionRangeProvider(options.documentSelector, provider), provider];
    }
}
exports.SelectionRangeFeature = SelectionRangeFeature;
//# sourceMappingURL=selectionRange.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/semanticTokens.js":
/*!************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/semanticTokens.js ***!
  \************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SemanticTokensFeature = void 0;
const vscode = __webpack_require__(/*! vscode */ "vscode");
const client_1 = __webpack_require__(/*! ./client */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const Is = __webpack_require__(/*! ./utils/is */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/is.js");
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class SemanticTokensFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.SemanticTokensRegistrationType.type);
    }
    fillClientCapabilities(capabilities) {
        const capability = ensure(ensure(capabilities, 'textDocument'), 'semanticTokens');
        capability.dynamicRegistration = true;
        capability.tokenTypes = [
            vscode_languageserver_protocol_1.SemanticTokenTypes.namespace,
            vscode_languageserver_protocol_1.SemanticTokenTypes.type,
            vscode_languageserver_protocol_1.SemanticTokenTypes.class,
            vscode_languageserver_protocol_1.SemanticTokenTypes.enum,
            vscode_languageserver_protocol_1.SemanticTokenTypes.interface,
            vscode_languageserver_protocol_1.SemanticTokenTypes.struct,
            vscode_languageserver_protocol_1.SemanticTokenTypes.typeParameter,
            vscode_languageserver_protocol_1.SemanticTokenTypes.parameter,
            vscode_languageserver_protocol_1.SemanticTokenTypes.variable,
            vscode_languageserver_protocol_1.SemanticTokenTypes.property,
            vscode_languageserver_protocol_1.SemanticTokenTypes.enumMember,
            vscode_languageserver_protocol_1.SemanticTokenTypes.event,
            vscode_languageserver_protocol_1.SemanticTokenTypes.function,
            vscode_languageserver_protocol_1.SemanticTokenTypes.method,
            vscode_languageserver_protocol_1.SemanticTokenTypes.macro,
            vscode_languageserver_protocol_1.SemanticTokenTypes.keyword,
            vscode_languageserver_protocol_1.SemanticTokenTypes.modifier,
            vscode_languageserver_protocol_1.SemanticTokenTypes.comment,
            vscode_languageserver_protocol_1.SemanticTokenTypes.string,
            vscode_languageserver_protocol_1.SemanticTokenTypes.number,
            vscode_languageserver_protocol_1.SemanticTokenTypes.regexp,
            vscode_languageserver_protocol_1.SemanticTokenTypes.operator
        ];
        capability.tokenModifiers = [
            vscode_languageserver_protocol_1.SemanticTokenModifiers.declaration,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.definition,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.readonly,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.static,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.deprecated,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.abstract,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.async,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.modification,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.documentation,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.defaultLibrary
        ];
        capability.formats = [vscode_languageserver_protocol_1.TokenFormat.Relative];
        capability.requests = {
            range: true,
            full: {
                delta: true
            }
        };
        capability.multilineTokenSupport = false;
        capability.overlappingTokenSupport = false;
        ensure(ensure(capabilities, 'workspace'), 'semanticTokens').refreshSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const client = this._client;
        client.onRequest(vscode_languageserver_protocol_1.SemanticTokensRefreshRequest.type, async () => {
            for (const provider of this.getAllProviders()) {
                provider.onDidChangeSemanticTokensEmitter.fire();
            }
        });
        const [id, options] = this.getRegistration(documentSelector, capabilities.semanticTokensProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const fullProvider = Is.boolean(options.full) ? options.full : options.full !== undefined;
        const hasEditProvider = options.full !== undefined && typeof options.full !== 'boolean' && options.full.delta === true;
        const eventEmitter = new vscode.EventEmitter();
        const documentProvider = fullProvider
            ? {
                onDidChangeSemanticTokens: eventEmitter.event,
                provideDocumentSemanticTokens: (document, token) => {
                    const client = this._client;
                    const middleware = client.clientOptions.middleware;
                    const provideDocumentSemanticTokens = (document, token) => {
                        const params = {
                            textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document)
                        };
                        return client.sendRequest(vscode_languageserver_protocol_1.SemanticTokensRequest.type, params, token).then((result) => {
                            return client.protocol2CodeConverter.asSemanticTokens(result);
                        }, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.SemanticTokensRequest.type, error, null);
                        });
                    };
                    return middleware.provideDocumentSemanticTokens
                        ? middleware.provideDocumentSemanticTokens(document, token, provideDocumentSemanticTokens)
                        : provideDocumentSemanticTokens(document, token);
                },
                provideDocumentSemanticTokensEdits: hasEditProvider
                    ? (document, previousResultId, token) => {
                        const client = this._client;
                        const middleware = client.clientOptions.middleware;
                        const provideDocumentSemanticTokensEdits = (document, previousResultId, token) => {
                            const params = {
                                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                                previousResultId
                            };
                            return client.sendRequest(vscode_languageserver_protocol_1.SemanticTokensDeltaRequest.type, params, token).then((result) => {
                                if (vscode_languageserver_protocol_1.SemanticTokens.is(result)) {
                                    return client.protocol2CodeConverter.asSemanticTokens(result);
                                }
                                else {
                                    return client.protocol2CodeConverter.asSemanticTokensEdits(result);
                                }
                            }, (error) => {
                                return client.handleFailedRequest(vscode_languageserver_protocol_1.SemanticTokensDeltaRequest.type, error, null);
                            });
                        };
                        return middleware.provideDocumentSemanticTokensEdits
                            ? middleware.provideDocumentSemanticTokensEdits(document, previousResultId, token, provideDocumentSemanticTokensEdits)
                            : provideDocumentSemanticTokensEdits(document, previousResultId, token);
                    }
                    : undefined
            }
            : undefined;
        const hasRangeProvider = options.range === true;
        const rangeProvider = hasRangeProvider
            ? {
                provideDocumentRangeSemanticTokens: (document, range, token) => {
                    const client = this._client;
                    const middleware = client.clientOptions.middleware;
                    const provideDocumentRangeSemanticTokens = (document, range, token) => {
                        const params = {
                            textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                            range: client.code2ProtocolConverter.asRange(range)
                        };
                        return client.sendRequest(vscode_languageserver_protocol_1.SemanticTokensRangeRequest.type, params, token).then((result) => {
                            return client.protocol2CodeConverter.asSemanticTokens(result);
                        }, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.SemanticTokensRangeRequest.type, error, null);
                        });
                    };
                    return middleware.provideDocumentRangeSemanticTokens
                        ? middleware.provideDocumentRangeSemanticTokens(document, range, token, provideDocumentRangeSemanticTokens)
                        : provideDocumentRangeSemanticTokens(document, range, token);
                }
            }
            : undefined;
        const disposables = [];
        const client = this._client;
        const legend = client.protocol2CodeConverter.asSemanticTokensLegend(options.legend);
        if (documentProvider !== undefined) {
            disposables.push(vscode.languages.registerDocumentSemanticTokensProvider(options.documentSelector, documentProvider, legend));
        }
        if (rangeProvider !== undefined) {
            disposables.push(vscode.languages.registerDocumentRangeSemanticTokensProvider(options.documentSelector, rangeProvider, legend));
        }
        return [new vscode.Disposable(() => disposables.forEach(item => item.dispose())), { range: rangeProvider, full: documentProvider, onDidChangeSemanticTokensEmitter: eventEmitter }];
    }
}
exports.SemanticTokensFeature = SemanticTokensFeature;
//# sourceMappingURL=semanticTokens.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/typeDefinition.js":
/*!************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/typeDefinition.js ***!
  \************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.TypeDefinitionFeature = void 0;
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
const client_1 = __webpack_require__(/*! ./client */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/client.js");
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
class TypeDefinitionFeature extends client_1.TextDocumentFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.TypeDefinitionRequest.type);
    }
    fillClientCapabilities(capabilities) {
        ensure(ensure(capabilities, 'textDocument'), 'typeDefinition').dynamicRegistration = true;
        let typeDefinitionSupport = ensure(ensure(capabilities, 'textDocument'), 'typeDefinition');
        typeDefinitionSupport.dynamicRegistration = true;
        typeDefinitionSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        let [id, options] = this.getRegistration(documentSelector, capabilities.typeDefinitionProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideTypeDefinition: (document, position, token) => {
                const client = this._client;
                const provideTypeDefinition = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.TypeDefinitionRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then(client.protocol2CodeConverter.asDefinitionResult, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.TypeDefinitionRequest.type, error, null);
                    });
                };
                const middleware = client.clientOptions.middleware;
                return middleware.provideTypeDefinition
                    ? middleware.provideTypeDefinition(document, position, token, provideTypeDefinition)
                    : provideTypeDefinition(document, position, token);
            }
        };
        return [vscode_1.languages.registerTypeDefinitionProvider(options.documentSelector, provider), provider];
    }
}
exports.TypeDefinitionFeature = TypeDefinitionFeature;
//# sourceMappingURL=typeDefinition.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/async.js":
/*!*********************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/async.js ***!
  \*********************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Delayer = void 0;
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
class Delayer {
    constructor(defaultDelay) {
        this.defaultDelay = defaultDelay;
        this.timeout = undefined;
        this.completionPromise = undefined;
        this.onSuccess = undefined;
        this.task = undefined;
    }
    trigger(task, delay = this.defaultDelay) {
        this.task = task;
        if (delay >= 0) {
            this.cancelTimeout();
        }
        if (!this.completionPromise) {
            this.completionPromise = new Promise((resolve) => {
                this.onSuccess = resolve;
            }).then(() => {
                this.completionPromise = undefined;
                this.onSuccess = undefined;
                var result = this.task();
                this.task = undefined;
                return result;
            });
        }
        if (delay >= 0 || this.timeout === void 0) {
            this.timeout = vscode_languageserver_protocol_1.RAL().timer.setTimeout(() => {
                this.timeout = undefined;
                this.onSuccess(undefined);
            }, delay >= 0 ? delay : this.defaultDelay);
        }
        return this.completionPromise;
    }
    forceDelivery() {
        if (!this.completionPromise) {
            return undefined;
        }
        this.cancelTimeout();
        let result = this.task();
        this.completionPromise = undefined;
        this.onSuccess = undefined;
        this.task = undefined;
        return result;
    }
    isTriggered() {
        return this.timeout !== void 0;
    }
    cancel() {
        this.cancelTimeout();
        this.completionPromise = undefined;
    }
    cancelTimeout() {
        if (this.timeout !== void 0) {
            vscode_languageserver_protocol_1.RAL().timer.clearTimeout(this.timeout);
            this.timeout = undefined;
        }
    }
}
exports.Delayer = Delayer;
//# sourceMappingURL=async.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/is.js":
/*!******************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/is.js ***!
  \******************************************************************************************/
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.asPromise = exports.thenable = exports.typedArray = exports.stringArray = exports.array = exports.func = exports.error = exports.number = exports.string = exports.boolean = void 0;
function boolean(value) {
    return value === true || value === false;
}
exports.boolean = boolean;
function string(value) {
    return typeof value === 'string' || value instanceof String;
}
exports.string = string;
function number(value) {
    return typeof value === 'number' || value instanceof Number;
}
exports.number = number;
function error(value) {
    return value instanceof Error;
}
exports.error = error;
function func(value) {
    return typeof value === 'function';
}
exports.func = func;
function array(value) {
    return Array.isArray(value);
}
exports.array = array;
function stringArray(value) {
    return array(value) && value.every(elem => string(elem));
}
exports.stringArray = stringArray;
function typedArray(value, check) {
    return Array.isArray(value) && value.every(check);
}
exports.typedArray = typedArray;
function thenable(value) {
    return value && func(value.then);
}
exports.thenable = thenable;
function asPromise(value) {
    if (value instanceof Promise) {
        return value;
    }
    else if (thenable(value)) {
        return new Promise((resolve, reject) => {
            value.then((resolved) => resolve(resolved), (error) => reject(error));
        });
    }
    else {
        return Promise.resolve(value);
    }
}
exports.asPromise = asPromise;
//# sourceMappingURL=is.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/uuid.js":
/*!********************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/uuid.js ***!
  \********************************************************************************************/
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.generateUuid = exports.parse = exports.isUUID = exports.v4 = exports.empty = void 0;
class ValueUUID {
    constructor(_value) {
        this._value = _value;
        // empty
    }
    asHex() {
        return this._value;
    }
    equals(other) {
        return this.asHex() === other.asHex();
    }
}
class V4UUID extends ValueUUID {
    constructor() {
        super([
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            '4',
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            V4UUID._oneOf(V4UUID._timeHighBits),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
        ].join(''));
    }
    static _oneOf(array) {
        return array[Math.floor(array.length * Math.random())];
    }
    static _randomHex() {
        return V4UUID._oneOf(V4UUID._chars);
    }
}
V4UUID._chars = ['0', '1', '2', '3', '4', '5', '6', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
V4UUID._timeHighBits = ['8', '9', 'a', 'b'];
/**
 * An empty UUID that contains only zeros.
 */
exports.empty = new ValueUUID('00000000-0000-0000-0000-000000000000');
function v4() {
    return new V4UUID();
}
exports.v4 = v4;
const _UUIDPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
function isUUID(value) {
    return _UUIDPattern.test(value);
}
exports.isUUID = isUUID;
/**
 * Parses a UUID that is of the format xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx.
 * @param value A uuid string.
 */
function parse(value) {
    if (!isUUID(value)) {
        throw new Error('invalid uuid');
    }
    return new ValueUUID(value);
}
exports.parse = parse;
function generateUuid() {
    return v4().asHex();
}
exports.generateUuid = generateUuid;
//# sourceMappingURL=uuid.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/workspaceFolders.js":
/*!**************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/lib/common/workspaceFolders.js ***!
  \**************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WorkspaceFoldersFeature = exports.arrayDiff = void 0;
const UUID = __webpack_require__(/*! ./utils/uuid */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/common/utils/uuid.js");
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const vscode_languageserver_protocol_1 = __webpack_require__(/*! vscode-languageserver-protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");
function access(target, key) {
    if (target === void 0) {
        return undefined;
    }
    return target[key];
}
function arrayDiff(left, right) {
    return left.filter(element => right.indexOf(element) < 0);
}
exports.arrayDiff = arrayDiff;
class WorkspaceFoldersFeature {
    constructor(_client) {
        this._client = _client;
        this._listeners = new Map();
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidChangeWorkspaceFoldersNotification.type;
    }
    fillInitializeParams(params) {
        const folders = vscode_1.workspace.workspaceFolders;
        this.initializeWithFolders(folders);
        if (folders === void 0) {
            params.workspaceFolders = null;
        }
        else {
            params.workspaceFolders = folders.map(folder => this.asProtocol(folder));
        }
    }
    initializeWithFolders(currentWorkspaceFolders) {
        this._initialFolders = currentWorkspaceFolders;
    }
    fillClientCapabilities(capabilities) {
        capabilities.workspace = capabilities.workspace || {};
        capabilities.workspace.workspaceFolders = true;
    }
    initialize(capabilities) {
        const client = this._client;
        client.onRequest(vscode_languageserver_protocol_1.WorkspaceFoldersRequest.type, (token) => {
            const workspaceFolders = () => {
                const folders = vscode_1.workspace.workspaceFolders;
                if (folders === undefined) {
                    return null;
                }
                const result = folders.map((folder) => {
                    return this.asProtocol(folder);
                });
                return result;
            };
            const middleware = client.clientOptions.middleware.workspace;
            return middleware && middleware.workspaceFolders
                ? middleware.workspaceFolders(token, workspaceFolders)
                : workspaceFolders(token);
        });
        const value = access(access(access(capabilities, 'workspace'), 'workspaceFolders'), 'changeNotifications');
        let id;
        if (typeof value === 'string') {
            id = value;
        }
        else if (value === true) {
            id = UUID.generateUuid();
        }
        if (id) {
            this.register({ id: id, registerOptions: undefined });
        }
    }
    sendInitialEvent(currentWorkspaceFolders) {
        if (this._initialFolders && currentWorkspaceFolders) {
            const removed = arrayDiff(this._initialFolders, currentWorkspaceFolders);
            const added = arrayDiff(currentWorkspaceFolders, this._initialFolders);
            if (added.length > 0 || removed.length > 0) {
                this.doSendEvent(added, removed);
            }
        }
        else if (this._initialFolders) {
            this.doSendEvent([], this._initialFolders);
        }
        else if (currentWorkspaceFolders) {
            this.doSendEvent(currentWorkspaceFolders, []);
        }
    }
    doSendEvent(addedFolders, removedFolders) {
        let params = {
            event: {
                added: addedFolders.map(folder => this.asProtocol(folder)),
                removed: removedFolders.map(folder => this.asProtocol(folder))
            }
        };
        this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeWorkspaceFoldersNotification.type, params);
    }
    register(data) {
        let id = data.id;
        let client = this._client;
        let disposable = vscode_1.workspace.onDidChangeWorkspaceFolders((event) => {
            let didChangeWorkspaceFolders = (event) => {
                this.doSendEvent(event.added, event.removed);
            };
            let middleware = client.clientOptions.middleware.workspace;
            middleware && middleware.didChangeWorkspaceFolders
                ? middleware.didChangeWorkspaceFolders(event, didChangeWorkspaceFolders)
                : didChangeWorkspaceFolders(event);
        });
        this._listeners.set(id, disposable);
        this.sendInitialEvent(vscode_1.workspace.workspaceFolders);
    }
    unregister(id) {
        let disposable = this._listeners.get(id);
        if (disposable === void 0) {
            return;
        }
        this._listeners.delete(id);
        disposable.dispose();
    }
    dispose() {
        for (let disposable of this._listeners.values()) {
            disposable.dispose();
        }
        this._listeners.clear();
    }
    asProtocol(workspaceFolder) {
        if (workspaceFolder === void 0) {
            return null;
        }
        return { uri: this._client.code2ProtocolConverter.asUri(workspaceFolder.uri), name: workspaceFolder.name };
    }
}
exports.WorkspaceFoldersFeature = WorkspaceFoldersFeature;
//# sourceMappingURL=workspaceFolders.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageclient/node.js":
/*!***************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageclient/node.js ***!
  \***************************************************************************/
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ----------------------------------------------------------------------------------------- */


module.exports = __webpack_require__(/*! ./lib/node/main */ "./src/lang-server/client/node_modules/vscode-languageclient/lib/browser/main.js");

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/browser.js":
/*!***************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/browser.js ***!
  \***************************************************************************************/
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ----------------------------------------------------------------------------------------- */


module.exports = __webpack_require__(/*! ./lib/browser/main */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js");

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js":
/*!************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/browser/main.js ***!
  \************************************************************************************************/
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createProtocolConnection = void 0;
const browser_1 = __webpack_require__(/*! vscode-jsonrpc/browser */ "./src/lang-server/client/node_modules/vscode-jsonrpc/browser.js");
__exportStar(__webpack_require__(/*! vscode-jsonrpc/browser */ "./src/lang-server/client/node_modules/vscode-jsonrpc/browser.js"), exports);
__exportStar(__webpack_require__(/*! ../common/api */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/api.js"), exports);
function createProtocolConnection(reader, writer, logger, options) {
    return browser_1.createMessageConnection(reader, writer, logger, options);
}
exports.createProtocolConnection = createProtocolConnection;
//# sourceMappingURL=main.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/api.js":
/*!**********************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/api.js ***!
  \**********************************************************************************************/
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.LSPErrorCodes = exports.createProtocolConnection = void 0;
__exportStar(__webpack_require__(/*! vscode-jsonrpc */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/browser/main.js"), exports);
__exportStar(__webpack_require__(/*! vscode-languageserver-types */ "./src/lang-server/client/node_modules/vscode-languageserver-types/lib/esm/main.js"), exports);
__exportStar(__webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js"), exports);
__exportStar(__webpack_require__(/*! ./protocol */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.js"), exports);
var connection_1 = __webpack_require__(/*! ./connection */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/connection.js");
Object.defineProperty(exports, "createProtocolConnection", ({ enumerable: true, get: function () { return connection_1.createProtocolConnection; } }));
var LSPErrorCodes;
(function (LSPErrorCodes) {
    /**
    * This is the start range of LSP reserved error codes.
    * It doesn't denote a real error code.
    *
    * @since 3.16.0
    */
    LSPErrorCodes.lspReservedErrorRangeStart = -32899;
    LSPErrorCodes.ContentModified = -32801;
    LSPErrorCodes.RequestCancelled = -32800;
    /**
    * This is the end range of LSP reserved error codes.
    * It doesn't denote a real error code.
    *
    * @since 3.16.0
    */
    LSPErrorCodes.lspReservedErrorRangeEnd = -32800;
})(LSPErrorCodes = exports.LSPErrorCodes || (exports.LSPErrorCodes = {}));
//# sourceMappingURL=api.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/connection.js":
/*!*****************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/connection.js ***!
  \*****************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createProtocolConnection = void 0;
const vscode_jsonrpc_1 = __webpack_require__(/*! vscode-jsonrpc */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/browser/main.js");
function createProtocolConnection(input, output, logger, options) {
    if (vscode_jsonrpc_1.ConnectionStrategy.is(options)) {
        options = { connectionStrategy: options };
    }
    return vscode_jsonrpc_1.createMessageConnection(input, output, logger, options);
}
exports.createProtocolConnection = createProtocolConnection;
//# sourceMappingURL=connection.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js":
/*!***************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js ***!
  \***************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ProtocolNotificationType = exports.ProtocolNotificationType0 = exports.ProtocolRequestType = exports.ProtocolRequestType0 = exports.RegistrationType = void 0;
const vscode_jsonrpc_1 = __webpack_require__(/*! vscode-jsonrpc */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/browser/main.js");
class RegistrationType {
    constructor(method) {
        this.method = method;
    }
}
exports.RegistrationType = RegistrationType;
class ProtocolRequestType0 extends vscode_jsonrpc_1.RequestType0 {
    constructor(method) {
        super(method);
    }
}
exports.ProtocolRequestType0 = ProtocolRequestType0;
class ProtocolRequestType extends vscode_jsonrpc_1.RequestType {
    constructor(method) {
        super(method, vscode_jsonrpc_1.ParameterStructures.byName);
    }
}
exports.ProtocolRequestType = ProtocolRequestType;
class ProtocolNotificationType0 extends vscode_jsonrpc_1.NotificationType0 {
    constructor(method) {
        super(method);
    }
}
exports.ProtocolNotificationType0 = ProtocolNotificationType0;
class ProtocolNotificationType extends vscode_jsonrpc_1.NotificationType {
    constructor(method) {
        super(method, vscode_jsonrpc_1.ParameterStructures.byName);
    }
}
exports.ProtocolNotificationType = ProtocolNotificationType;
// let x: ProtocolNotificationType<number, { value: number}>;
// let y: ProtocolNotificationType<string, { value: number}>;
// x = y;
//# sourceMappingURL=messages.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.callHierarchy.js":
/*!*****************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.callHierarchy.js ***!
  \*****************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) TypeFox and others. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.CallHierarchyOutgoingCallsRequest = exports.CallHierarchyIncomingCallsRequest = exports.CallHierarchyPrepareRequest = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
/**
 * A request to result a `CallHierarchyItem` in a document at a given position.
 * Can be used as an input to a incoming or outgoing call hierarchy.
 *
 * @since 3.16.0
 */
var CallHierarchyPrepareRequest;
(function (CallHierarchyPrepareRequest) {
    CallHierarchyPrepareRequest.method = 'textDocument/prepareCallHierarchy';
    CallHierarchyPrepareRequest.type = new messages_1.ProtocolRequestType(CallHierarchyPrepareRequest.method);
})(CallHierarchyPrepareRequest = exports.CallHierarchyPrepareRequest || (exports.CallHierarchyPrepareRequest = {}));
/**
 * A request to resolve the incoming calls for a given `CallHierarchyItem`.
 *
 * @since 3.16.0
 */
var CallHierarchyIncomingCallsRequest;
(function (CallHierarchyIncomingCallsRequest) {
    CallHierarchyIncomingCallsRequest.method = 'callHierarchy/incomingCalls';
    CallHierarchyIncomingCallsRequest.type = new messages_1.ProtocolRequestType(CallHierarchyIncomingCallsRequest.method);
})(CallHierarchyIncomingCallsRequest = exports.CallHierarchyIncomingCallsRequest || (exports.CallHierarchyIncomingCallsRequest = {}));
/**
 * A request to resolve the outgoing calls for a given `CallHierarchyItem`.
 *
 * @since 3.16.0
 */
var CallHierarchyOutgoingCallsRequest;
(function (CallHierarchyOutgoingCallsRequest) {
    CallHierarchyOutgoingCallsRequest.method = 'callHierarchy/outgoingCalls';
    CallHierarchyOutgoingCallsRequest.type = new messages_1.ProtocolRequestType(CallHierarchyOutgoingCallsRequest.method);
})(CallHierarchyOutgoingCallsRequest = exports.CallHierarchyOutgoingCallsRequest || (exports.CallHierarchyOutgoingCallsRequest = {}));
//# sourceMappingURL=protocol.callHierarchy.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.colorProvider.js":
/*!*****************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.colorProvider.js ***!
  \*****************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ColorPresentationRequest = exports.DocumentColorRequest = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
/**
 * A request to list all color symbols found in a given text document. The request's
 * parameter is of type [DocumentColorParams](#DocumentColorParams) the
 * response is of type [ColorInformation[]](#ColorInformation) or a Thenable
 * that resolves to such.
 */
var DocumentColorRequest;
(function (DocumentColorRequest) {
    DocumentColorRequest.method = 'textDocument/documentColor';
    DocumentColorRequest.type = new messages_1.ProtocolRequestType(DocumentColorRequest.method);
})(DocumentColorRequest = exports.DocumentColorRequest || (exports.DocumentColorRequest = {}));
/**
 * A request to list all presentation for a color. The request's
 * parameter is of type [ColorPresentationParams](#ColorPresentationParams) the
 * response is of type [ColorInformation[]](#ColorInformation) or a Thenable
 * that resolves to such.
 */
var ColorPresentationRequest;
(function (ColorPresentationRequest) {
    ColorPresentationRequest.type = new messages_1.ProtocolRequestType('textDocument/colorPresentation');
})(ColorPresentationRequest = exports.ColorPresentationRequest || (exports.ColorPresentationRequest = {}));
//# sourceMappingURL=protocol.colorProvider.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.configuration.js":
/*!*****************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.configuration.js ***!
  \*****************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ConfigurationRequest = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
/**
 * The 'workspace/configuration' request is sent from the server to the client to fetch a certain
 * configuration setting.
 *
 * This pull model replaces the old push model were the client signaled configuration change via an
 * event. If the server still needs to react to configuration changes (since the server caches the
 * result of `workspace/configuration` requests) the server should register for an empty configuration
 * change event and empty the cache if such an event is received.
 */
var ConfigurationRequest;
(function (ConfigurationRequest) {
    ConfigurationRequest.type = new messages_1.ProtocolRequestType('workspace/configuration');
})(ConfigurationRequest = exports.ConfigurationRequest || (exports.ConfigurationRequest = {}));
//# sourceMappingURL=protocol.configuration.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.declaration.js":
/*!***************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.declaration.js ***!
  \***************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DeclarationRequest = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
// @ts-ignore: to avoid inlining LocatioLink as dynamic import
let __noDynamicImport;
/**
 * A request to resolve the type definition locations of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPositioParams]
 * (#TextDocumentPositionParams) the response is of type [Declaration](#Declaration)
 * or a typed array of [DeclarationLink](#DeclarationLink) or a Thenable that resolves
 * to such.
 */
var DeclarationRequest;
(function (DeclarationRequest) {
    DeclarationRequest.method = 'textDocument/declaration';
    DeclarationRequest.type = new messages_1.ProtocolRequestType(DeclarationRequest.method);
})(DeclarationRequest = exports.DeclarationRequest || (exports.DeclarationRequest = {}));
//# sourceMappingURL=protocol.declaration.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.fileOperations.js":
/*!******************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.fileOperations.js ***!
  \******************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WillDeleteFilesRequest = exports.DidDeleteFilesNotification = exports.DidRenameFilesNotification = exports.WillRenameFilesRequest = exports.DidCreateFilesNotification = exports.WillCreateFilesRequest = exports.FileOperationPatternKind = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
/**
 * A pattern kind describing if a glob pattern matches a file a folder or
 * both.
 *
 * @since 3.16.0
 */
var FileOperationPatternKind;
(function (FileOperationPatternKind) {
    /**
     * The pattern matches a file only.
     */
    FileOperationPatternKind.file = 'file';
    /**
     * The pattern matches a folder only.
     */
    FileOperationPatternKind.folder = 'folder';
})(FileOperationPatternKind = exports.FileOperationPatternKind || (exports.FileOperationPatternKind = {}));
/**
 * The will create files request is sent from the client to the server before files are actually
 * created as long as the creation is triggered from within the client.
 *
 * @since 3.16.0
 */
var WillCreateFilesRequest;
(function (WillCreateFilesRequest) {
    WillCreateFilesRequest.method = 'workspace/willCreateFiles';
    WillCreateFilesRequest.type = new messages_1.ProtocolRequestType(WillCreateFilesRequest.method);
})(WillCreateFilesRequest = exports.WillCreateFilesRequest || (exports.WillCreateFilesRequest = {}));
/**
 * The did create files notification is sent from the client to the server when
 * files were created from within the client.
 *
 * @since 3.16.0
 */
var DidCreateFilesNotification;
(function (DidCreateFilesNotification) {
    DidCreateFilesNotification.method = 'workspace/didCreateFiles';
    DidCreateFilesNotification.type = new messages_1.ProtocolNotificationType(DidCreateFilesNotification.method);
})(DidCreateFilesNotification = exports.DidCreateFilesNotification || (exports.DidCreateFilesNotification = {}));
/**
 * The will rename files request is sent from the client to the server before files are actually
 * renamed as long as the rename is triggered from within the client.
 *
 * @since 3.16.0
 */
var WillRenameFilesRequest;
(function (WillRenameFilesRequest) {
    WillRenameFilesRequest.method = 'workspace/willRenameFiles';
    WillRenameFilesRequest.type = new messages_1.ProtocolRequestType(WillRenameFilesRequest.method);
})(WillRenameFilesRequest = exports.WillRenameFilesRequest || (exports.WillRenameFilesRequest = {}));
/**
 * The did rename files notification is sent from the client to the server when
 * files were renamed from within the client.
 *
 * @since 3.16.0
 */
var DidRenameFilesNotification;
(function (DidRenameFilesNotification) {
    DidRenameFilesNotification.method = 'workspace/didRenameFiles';
    DidRenameFilesNotification.type = new messages_1.ProtocolNotificationType(DidRenameFilesNotification.method);
})(DidRenameFilesNotification = exports.DidRenameFilesNotification || (exports.DidRenameFilesNotification = {}));
/**
 * The will delete files request is sent from the client to the server before files are actually
 * deleted as long as the deletion is triggered from within the client.
 *
 * @since 3.16.0
 */
var DidDeleteFilesNotification;
(function (DidDeleteFilesNotification) {
    DidDeleteFilesNotification.method = 'workspace/didDeleteFiles';
    DidDeleteFilesNotification.type = new messages_1.ProtocolNotificationType(DidDeleteFilesNotification.method);
})(DidDeleteFilesNotification = exports.DidDeleteFilesNotification || (exports.DidDeleteFilesNotification = {}));
/**
 * The did delete files notification is sent from the client to the server when
 * files were deleted from within the client.
 *
 * @since 3.16.0
 */
var WillDeleteFilesRequest;
(function (WillDeleteFilesRequest) {
    WillDeleteFilesRequest.method = 'workspace/willDeleteFiles';
    WillDeleteFilesRequest.type = new messages_1.ProtocolRequestType(WillDeleteFilesRequest.method);
})(WillDeleteFilesRequest = exports.WillDeleteFilesRequest || (exports.WillDeleteFilesRequest = {}));
//# sourceMappingURL=protocol.fileOperations.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.foldingRange.js":
/*!****************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.foldingRange.js ***!
  \****************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.FoldingRangeRequest = exports.FoldingRangeKind = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
/**
 * Enum of known range kinds
 */
var FoldingRangeKind;
(function (FoldingRangeKind) {
    /**
     * Folding range for a comment
     */
    FoldingRangeKind["Comment"] = "comment";
    /**
     * Folding range for a imports or includes
     */
    FoldingRangeKind["Imports"] = "imports";
    /**
     * Folding range for a region (e.g. `#region`)
     */
    FoldingRangeKind["Region"] = "region";
})(FoldingRangeKind = exports.FoldingRangeKind || (exports.FoldingRangeKind = {}));
/**
 * A request to provide folding ranges in a document. The request's
 * parameter is of type [FoldingRangeParams](#FoldingRangeParams), the
 * response is of type [FoldingRangeList](#FoldingRangeList) or a Thenable
 * that resolves to such.
 */
var FoldingRangeRequest;
(function (FoldingRangeRequest) {
    FoldingRangeRequest.method = 'textDocument/foldingRange';
    FoldingRangeRequest.type = new messages_1.ProtocolRequestType(FoldingRangeRequest.method);
})(FoldingRangeRequest = exports.FoldingRangeRequest || (exports.FoldingRangeRequest = {}));
//# sourceMappingURL=protocol.foldingRange.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.implementation.js":
/*!******************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.implementation.js ***!
  \******************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ImplementationRequest = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
// @ts-ignore: to avoid inlining LocatioLink as dynamic import
let __noDynamicImport;
/**
 * A request to resolve the implementation locations of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPositioParams]
 * (#TextDocumentPositionParams) the response is of type [Definition](#Definition) or a
 * Thenable that resolves to such.
 */
var ImplementationRequest;
(function (ImplementationRequest) {
    ImplementationRequest.method = 'textDocument/implementation';
    ImplementationRequest.type = new messages_1.ProtocolRequestType(ImplementationRequest.method);
})(ImplementationRequest = exports.ImplementationRequest || (exports.ImplementationRequest = {}));
//# sourceMappingURL=protocol.implementation.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.js":
/*!***************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.js ***!
  \***************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DocumentLinkRequest = exports.CodeLensRefreshRequest = exports.CodeLensResolveRequest = exports.CodeLensRequest = exports.WorkspaceSymbolRequest = exports.CodeActionResolveRequest = exports.CodeActionRequest = exports.DocumentSymbolRequest = exports.DocumentHighlightRequest = exports.ReferencesRequest = exports.DefinitionRequest = exports.SignatureHelpRequest = exports.SignatureHelpTriggerKind = exports.HoverRequest = exports.CompletionResolveRequest = exports.CompletionRequest = exports.CompletionTriggerKind = exports.PublishDiagnosticsNotification = exports.WatchKind = exports.FileChangeType = exports.DidChangeWatchedFilesNotification = exports.WillSaveTextDocumentWaitUntilRequest = exports.WillSaveTextDocumentNotification = exports.TextDocumentSaveReason = exports.DidSaveTextDocumentNotification = exports.DidCloseTextDocumentNotification = exports.DidChangeTextDocumentNotification = exports.TextDocumentContentChangeEvent = exports.DidOpenTextDocumentNotification = exports.TextDocumentSyncKind = exports.TelemetryEventNotification = exports.LogMessageNotification = exports.ShowMessageRequest = exports.ShowMessageNotification = exports.MessageType = exports.DidChangeConfigurationNotification = exports.ExitNotification = exports.ShutdownRequest = exports.InitializedNotification = exports.InitializeError = exports.InitializeRequest = exports.WorkDoneProgressOptions = exports.TextDocumentRegistrationOptions = exports.StaticRegistrationOptions = exports.FailureHandlingKind = exports.ResourceOperationKind = exports.UnregistrationRequest = exports.RegistrationRequest = exports.DocumentSelector = exports.DocumentFilter = void 0;
exports.MonikerRequest = exports.MonikerKind = exports.UniquenessLevel = exports.WillDeleteFilesRequest = exports.DidDeleteFilesNotification = exports.WillRenameFilesRequest = exports.DidRenameFilesNotification = exports.WillCreateFilesRequest = exports.DidCreateFilesNotification = exports.FileOperationPatternKind = exports.LinkedEditingRangeRequest = exports.ShowDocumentRequest = exports.SemanticTokensRegistrationType = exports.SemanticTokensRefreshRequest = exports.SemanticTokensRangeRequest = exports.SemanticTokensDeltaRequest = exports.SemanticTokensRequest = exports.TokenFormat = exports.SemanticTokens = exports.SemanticTokenModifiers = exports.SemanticTokenTypes = exports.CallHierarchyPrepareRequest = exports.CallHierarchyOutgoingCallsRequest = exports.CallHierarchyIncomingCallsRequest = exports.WorkDoneProgressCancelNotification = exports.WorkDoneProgressCreateRequest = exports.WorkDoneProgress = exports.SelectionRangeRequest = exports.DeclarationRequest = exports.FoldingRangeRequest = exports.ColorPresentationRequest = exports.DocumentColorRequest = exports.ConfigurationRequest = exports.DidChangeWorkspaceFoldersNotification = exports.WorkspaceFoldersRequest = exports.TypeDefinitionRequest = exports.ImplementationRequest = exports.ApplyWorkspaceEditRequest = exports.ExecuteCommandRequest = exports.PrepareRenameRequest = exports.RenameRequest = exports.PrepareSupportDefaultBehavior = exports.DocumentOnTypeFormattingRequest = exports.DocumentRangeFormattingRequest = exports.DocumentFormattingRequest = exports.DocumentLinkResolveRequest = void 0;
const Is = __webpack_require__(/*! ./utils/is */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/utils/is.js");
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
const protocol_implementation_1 = __webpack_require__(/*! ./protocol.implementation */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.implementation.js");
Object.defineProperty(exports, "ImplementationRequest", ({ enumerable: true, get: function () { return protocol_implementation_1.ImplementationRequest; } }));
const protocol_typeDefinition_1 = __webpack_require__(/*! ./protocol.typeDefinition */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.typeDefinition.js");
Object.defineProperty(exports, "TypeDefinitionRequest", ({ enumerable: true, get: function () { return protocol_typeDefinition_1.TypeDefinitionRequest; } }));
const protocol_workspaceFolders_1 = __webpack_require__(/*! ./protocol.workspaceFolders */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.workspaceFolders.js");
Object.defineProperty(exports, "WorkspaceFoldersRequest", ({ enumerable: true, get: function () { return protocol_workspaceFolders_1.WorkspaceFoldersRequest; } }));
Object.defineProperty(exports, "DidChangeWorkspaceFoldersNotification", ({ enumerable: true, get: function () { return protocol_workspaceFolders_1.DidChangeWorkspaceFoldersNotification; } }));
const protocol_configuration_1 = __webpack_require__(/*! ./protocol.configuration */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.configuration.js");
Object.defineProperty(exports, "ConfigurationRequest", ({ enumerable: true, get: function () { return protocol_configuration_1.ConfigurationRequest; } }));
const protocol_colorProvider_1 = __webpack_require__(/*! ./protocol.colorProvider */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.colorProvider.js");
Object.defineProperty(exports, "DocumentColorRequest", ({ enumerable: true, get: function () { return protocol_colorProvider_1.DocumentColorRequest; } }));
Object.defineProperty(exports, "ColorPresentationRequest", ({ enumerable: true, get: function () { return protocol_colorProvider_1.ColorPresentationRequest; } }));
const protocol_foldingRange_1 = __webpack_require__(/*! ./protocol.foldingRange */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.foldingRange.js");
Object.defineProperty(exports, "FoldingRangeRequest", ({ enumerable: true, get: function () { return protocol_foldingRange_1.FoldingRangeRequest; } }));
const protocol_declaration_1 = __webpack_require__(/*! ./protocol.declaration */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.declaration.js");
Object.defineProperty(exports, "DeclarationRequest", ({ enumerable: true, get: function () { return protocol_declaration_1.DeclarationRequest; } }));
const protocol_selectionRange_1 = __webpack_require__(/*! ./protocol.selectionRange */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.selectionRange.js");
Object.defineProperty(exports, "SelectionRangeRequest", ({ enumerable: true, get: function () { return protocol_selectionRange_1.SelectionRangeRequest; } }));
const protocol_progress_1 = __webpack_require__(/*! ./protocol.progress */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.progress.js");
Object.defineProperty(exports, "WorkDoneProgress", ({ enumerable: true, get: function () { return protocol_progress_1.WorkDoneProgress; } }));
Object.defineProperty(exports, "WorkDoneProgressCreateRequest", ({ enumerable: true, get: function () { return protocol_progress_1.WorkDoneProgressCreateRequest; } }));
Object.defineProperty(exports, "WorkDoneProgressCancelNotification", ({ enumerable: true, get: function () { return protocol_progress_1.WorkDoneProgressCancelNotification; } }));
const protocol_callHierarchy_1 = __webpack_require__(/*! ./protocol.callHierarchy */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.callHierarchy.js");
Object.defineProperty(exports, "CallHierarchyIncomingCallsRequest", ({ enumerable: true, get: function () { return protocol_callHierarchy_1.CallHierarchyIncomingCallsRequest; } }));
Object.defineProperty(exports, "CallHierarchyOutgoingCallsRequest", ({ enumerable: true, get: function () { return protocol_callHierarchy_1.CallHierarchyOutgoingCallsRequest; } }));
Object.defineProperty(exports, "CallHierarchyPrepareRequest", ({ enumerable: true, get: function () { return protocol_callHierarchy_1.CallHierarchyPrepareRequest; } }));
const protocol_semanticTokens_1 = __webpack_require__(/*! ./protocol.semanticTokens */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.semanticTokens.js");
Object.defineProperty(exports, "SemanticTokenTypes", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokenTypes; } }));
Object.defineProperty(exports, "SemanticTokenModifiers", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokenModifiers; } }));
Object.defineProperty(exports, "SemanticTokens", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokens; } }));
Object.defineProperty(exports, "TokenFormat", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.TokenFormat; } }));
Object.defineProperty(exports, "SemanticTokensRequest", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokensRequest; } }));
Object.defineProperty(exports, "SemanticTokensDeltaRequest", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokensDeltaRequest; } }));
Object.defineProperty(exports, "SemanticTokensRangeRequest", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokensRangeRequest; } }));
Object.defineProperty(exports, "SemanticTokensRefreshRequest", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokensRefreshRequest; } }));
Object.defineProperty(exports, "SemanticTokensRegistrationType", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokensRegistrationType; } }));
const protocol_showDocument_1 = __webpack_require__(/*! ./protocol.showDocument */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.showDocument.js");
Object.defineProperty(exports, "ShowDocumentRequest", ({ enumerable: true, get: function () { return protocol_showDocument_1.ShowDocumentRequest; } }));
const protocol_linkedEditingRange_1 = __webpack_require__(/*! ./protocol.linkedEditingRange */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.linkedEditingRange.js");
Object.defineProperty(exports, "LinkedEditingRangeRequest", ({ enumerable: true, get: function () { return protocol_linkedEditingRange_1.LinkedEditingRangeRequest; } }));
const protocol_fileOperations_1 = __webpack_require__(/*! ./protocol.fileOperations */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.fileOperations.js");
Object.defineProperty(exports, "FileOperationPatternKind", ({ enumerable: true, get: function () { return protocol_fileOperations_1.FileOperationPatternKind; } }));
Object.defineProperty(exports, "DidCreateFilesNotification", ({ enumerable: true, get: function () { return protocol_fileOperations_1.DidCreateFilesNotification; } }));
Object.defineProperty(exports, "WillCreateFilesRequest", ({ enumerable: true, get: function () { return protocol_fileOperations_1.WillCreateFilesRequest; } }));
Object.defineProperty(exports, "DidRenameFilesNotification", ({ enumerable: true, get: function () { return protocol_fileOperations_1.DidRenameFilesNotification; } }));
Object.defineProperty(exports, "WillRenameFilesRequest", ({ enumerable: true, get: function () { return protocol_fileOperations_1.WillRenameFilesRequest; } }));
Object.defineProperty(exports, "DidDeleteFilesNotification", ({ enumerable: true, get: function () { return protocol_fileOperations_1.DidDeleteFilesNotification; } }));
Object.defineProperty(exports, "WillDeleteFilesRequest", ({ enumerable: true, get: function () { return protocol_fileOperations_1.WillDeleteFilesRequest; } }));
const protocol_moniker_1 = __webpack_require__(/*! ./protocol.moniker */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.moniker.js");
Object.defineProperty(exports, "UniquenessLevel", ({ enumerable: true, get: function () { return protocol_moniker_1.UniquenessLevel; } }));
Object.defineProperty(exports, "MonikerKind", ({ enumerable: true, get: function () { return protocol_moniker_1.MonikerKind; } }));
Object.defineProperty(exports, "MonikerRequest", ({ enumerable: true, get: function () { return protocol_moniker_1.MonikerRequest; } }));
// @ts-ignore: to avoid inlining LocationLink as dynamic import
let __noDynamicImport;
/**
 * The DocumentFilter namespace provides helper functions to work with
 * [DocumentFilter](#DocumentFilter) literals.
 */
var DocumentFilter;
(function (DocumentFilter) {
    function is(value) {
        const candidate = value;
        return Is.string(candidate.language) || Is.string(candidate.scheme) || Is.string(candidate.pattern);
    }
    DocumentFilter.is = is;
})(DocumentFilter = exports.DocumentFilter || (exports.DocumentFilter = {}));
/**
 * The DocumentSelector namespace provides helper functions to work with
 * [DocumentSelector](#DocumentSelector)s.
 */
var DocumentSelector;
(function (DocumentSelector) {
    function is(value) {
        if (!Array.isArray(value)) {
            return false;
        }
        for (let elem of value) {
            if (!Is.string(elem) && !DocumentFilter.is(elem)) {
                return false;
            }
        }
        return true;
    }
    DocumentSelector.is = is;
})(DocumentSelector = exports.DocumentSelector || (exports.DocumentSelector = {}));
/**
 * The `client/registerCapability` request is sent from the server to the client to register a new capability
 * handler on the client side.
 */
var RegistrationRequest;
(function (RegistrationRequest) {
    RegistrationRequest.type = new messages_1.ProtocolRequestType('client/registerCapability');
})(RegistrationRequest = exports.RegistrationRequest || (exports.RegistrationRequest = {}));
/**
 * The `client/unregisterCapability` request is sent from the server to the client to unregister a previously registered capability
 * handler on the client side.
 */
var UnregistrationRequest;
(function (UnregistrationRequest) {
    UnregistrationRequest.type = new messages_1.ProtocolRequestType('client/unregisterCapability');
})(UnregistrationRequest = exports.UnregistrationRequest || (exports.UnregistrationRequest = {}));
var ResourceOperationKind;
(function (ResourceOperationKind) {
    /**
     * Supports creating new files and folders.
     */
    ResourceOperationKind.Create = 'create';
    /**
     * Supports renaming existing files and folders.
     */
    ResourceOperationKind.Rename = 'rename';
    /**
     * Supports deleting existing files and folders.
     */
    ResourceOperationKind.Delete = 'delete';
})(ResourceOperationKind = exports.ResourceOperationKind || (exports.ResourceOperationKind = {}));
var FailureHandlingKind;
(function (FailureHandlingKind) {
    /**
     * Applying the workspace change is simply aborted if one of the changes provided
     * fails. All operations executed before the failing operation stay executed.
     */
    FailureHandlingKind.Abort = 'abort';
    /**
     * All operations are executed transactional. That means they either all
     * succeed or no changes at all are applied to the workspace.
     */
    FailureHandlingKind.Transactional = 'transactional';
    /**
     * If the workspace edit contains only textual file changes they are executed transactional.
     * If resource changes (create, rename or delete file) are part of the change the failure
     * handling strategy is abort.
     */
    FailureHandlingKind.TextOnlyTransactional = 'textOnlyTransactional';
    /**
     * The client tries to undo the operations already executed. But there is no
     * guarantee that this is succeeding.
     */
    FailureHandlingKind.Undo = 'undo';
})(FailureHandlingKind = exports.FailureHandlingKind || (exports.FailureHandlingKind = {}));
/**
 * The StaticRegistrationOptions namespace provides helper functions to work with
 * [StaticRegistrationOptions](#StaticRegistrationOptions) literals.
 */
var StaticRegistrationOptions;
(function (StaticRegistrationOptions) {
    function hasId(value) {
        const candidate = value;
        return candidate && Is.string(candidate.id) && candidate.id.length > 0;
    }
    StaticRegistrationOptions.hasId = hasId;
})(StaticRegistrationOptions = exports.StaticRegistrationOptions || (exports.StaticRegistrationOptions = {}));
/**
 * The TextDocumentRegistrationOptions namespace provides helper functions to work with
 * [TextDocumentRegistrationOptions](#TextDocumentRegistrationOptions) literals.
 */
var TextDocumentRegistrationOptions;
(function (TextDocumentRegistrationOptions) {
    function is(value) {
        const candidate = value;
        return candidate && (candidate.documentSelector === null || DocumentSelector.is(candidate.documentSelector));
    }
    TextDocumentRegistrationOptions.is = is;
})(TextDocumentRegistrationOptions = exports.TextDocumentRegistrationOptions || (exports.TextDocumentRegistrationOptions = {}));
/**
 * The WorkDoneProgressOptions namespace provides helper functions to work with
 * [WorkDoneProgressOptions](#WorkDoneProgressOptions) literals.
 */
var WorkDoneProgressOptions;
(function (WorkDoneProgressOptions) {
    function is(value) {
        const candidate = value;
        return Is.objectLiteral(candidate) && (candidate.workDoneProgress === undefined || Is.boolean(candidate.workDoneProgress));
    }
    WorkDoneProgressOptions.is = is;
    function hasWorkDoneProgress(value) {
        const candidate = value;
        return candidate && Is.boolean(candidate.workDoneProgress);
    }
    WorkDoneProgressOptions.hasWorkDoneProgress = hasWorkDoneProgress;
})(WorkDoneProgressOptions = exports.WorkDoneProgressOptions || (exports.WorkDoneProgressOptions = {}));
/**
 * The initialize request is sent from the client to the server.
 * It is sent once as the request after starting up the server.
 * The requests parameter is of type [InitializeParams](#InitializeParams)
 * the response if of type [InitializeResult](#InitializeResult) of a Thenable that
 * resolves to such.
 */
var InitializeRequest;
(function (InitializeRequest) {
    InitializeRequest.type = new messages_1.ProtocolRequestType('initialize');
})(InitializeRequest = exports.InitializeRequest || (exports.InitializeRequest = {}));
/**
 * Known error codes for an `InitializeError`;
 */
var InitializeError;
(function (InitializeError) {
    /**
     * If the protocol version provided by the client can't be handled by the server.
     * @deprecated This initialize error got replaced by client capabilities. There is
     * no version handshake in version 3.0x
     */
    InitializeError.unknownProtocolVersion = 1;
})(InitializeError = exports.InitializeError || (exports.InitializeError = {}));
/**
 * The initialized notification is sent from the client to the
 * server after the client is fully initialized and the server
 * is allowed to send requests from the server to the client.
 */
var InitializedNotification;
(function (InitializedNotification) {
    InitializedNotification.type = new messages_1.ProtocolNotificationType('initialized');
})(InitializedNotification = exports.InitializedNotification || (exports.InitializedNotification = {}));
//---- Shutdown Method ----
/**
 * A shutdown request is sent from the client to the server.
 * It is sent once when the client decides to shutdown the
 * server. The only notification that is sent after a shutdown request
 * is the exit event.
 */
var ShutdownRequest;
(function (ShutdownRequest) {
    ShutdownRequest.type = new messages_1.ProtocolRequestType0('shutdown');
})(ShutdownRequest = exports.ShutdownRequest || (exports.ShutdownRequest = {}));
//---- Exit Notification ----
/**
 * The exit event is sent from the client to the server to
 * ask the server to exit its process.
 */
var ExitNotification;
(function (ExitNotification) {
    ExitNotification.type = new messages_1.ProtocolNotificationType0('exit');
})(ExitNotification = exports.ExitNotification || (exports.ExitNotification = {}));
/**
 * The configuration change notification is sent from the client to the server
 * when the client's configuration has changed. The notification contains
 * the changed configuration as defined by the language client.
 */
var DidChangeConfigurationNotification;
(function (DidChangeConfigurationNotification) {
    DidChangeConfigurationNotification.type = new messages_1.ProtocolNotificationType('workspace/didChangeConfiguration');
})(DidChangeConfigurationNotification = exports.DidChangeConfigurationNotification || (exports.DidChangeConfigurationNotification = {}));
//---- Message show and log notifications ----
/**
 * The message type
 */
var MessageType;
(function (MessageType) {
    /**
     * An error message.
     */
    MessageType.Error = 1;
    /**
     * A warning message.
     */
    MessageType.Warning = 2;
    /**
     * An information message.
     */
    MessageType.Info = 3;
    /**
     * A log message.
     */
    MessageType.Log = 4;
})(MessageType = exports.MessageType || (exports.MessageType = {}));
/**
 * The show message notification is sent from a server to a client to ask
 * the client to display a particular message in the user interface.
 */
var ShowMessageNotification;
(function (ShowMessageNotification) {
    ShowMessageNotification.type = new messages_1.ProtocolNotificationType('window/showMessage');
})(ShowMessageNotification = exports.ShowMessageNotification || (exports.ShowMessageNotification = {}));
/**
 * The show message request is sent from the server to the client to show a message
 * and a set of options actions to the user.
 */
var ShowMessageRequest;
(function (ShowMessageRequest) {
    ShowMessageRequest.type = new messages_1.ProtocolRequestType('window/showMessageRequest');
})(ShowMessageRequest = exports.ShowMessageRequest || (exports.ShowMessageRequest = {}));
/**
 * The log message notification is sent from the server to the client to ask
 * the client to log a particular message.
 */
var LogMessageNotification;
(function (LogMessageNotification) {
    LogMessageNotification.type = new messages_1.ProtocolNotificationType('window/logMessage');
})(LogMessageNotification = exports.LogMessageNotification || (exports.LogMessageNotification = {}));
//---- Telemetry notification
/**
 * The telemetry event notification is sent from the server to the client to ask
 * the client to log telemetry data.
 */
var TelemetryEventNotification;
(function (TelemetryEventNotification) {
    TelemetryEventNotification.type = new messages_1.ProtocolNotificationType('telemetry/event');
})(TelemetryEventNotification = exports.TelemetryEventNotification || (exports.TelemetryEventNotification = {}));
/**
 * Defines how the host (editor) should sync
 * document changes to the language server.
 */
var TextDocumentSyncKind;
(function (TextDocumentSyncKind) {
    /**
     * Documents should not be synced at all.
     */
    TextDocumentSyncKind.None = 0;
    /**
     * Documents are synced by always sending the full content
     * of the document.
     */
    TextDocumentSyncKind.Full = 1;
    /**
     * Documents are synced by sending the full content on open.
     * After that only incremental updates to the document are
     * send.
     */
    TextDocumentSyncKind.Incremental = 2;
})(TextDocumentSyncKind = exports.TextDocumentSyncKind || (exports.TextDocumentSyncKind = {}));
/**
 * The document open notification is sent from the client to the server to signal
 * newly opened text documents. The document's truth is now managed by the client
 * and the server must not try to read the document's truth using the document's
 * uri. Open in this sense means it is managed by the client. It doesn't necessarily
 * mean that its content is presented in an editor. An open notification must not
 * be sent more than once without a corresponding close notification send before.
 * This means open and close notification must be balanced and the max open count
 * is one.
 */
var DidOpenTextDocumentNotification;
(function (DidOpenTextDocumentNotification) {
    DidOpenTextDocumentNotification.method = 'textDocument/didOpen';
    DidOpenTextDocumentNotification.type = new messages_1.ProtocolNotificationType(DidOpenTextDocumentNotification.method);
})(DidOpenTextDocumentNotification = exports.DidOpenTextDocumentNotification || (exports.DidOpenTextDocumentNotification = {}));
var TextDocumentContentChangeEvent;
(function (TextDocumentContentChangeEvent) {
    /**
     * Checks whether the information describes a delta event.
     */
    function isIncremental(event) {
        let candidate = event;
        return candidate !== undefined && candidate !== null &&
            typeof candidate.text === 'string' && candidate.range !== undefined &&
            (candidate.rangeLength === undefined || typeof candidate.rangeLength === 'number');
    }
    TextDocumentContentChangeEvent.isIncremental = isIncremental;
    /**
     * Checks whether the information describes a full replacement event.
     */
    function isFull(event) {
        let candidate = event;
        return candidate !== undefined && candidate !== null &&
            typeof candidate.text === 'string' && candidate.range === undefined && candidate.rangeLength === undefined;
    }
    TextDocumentContentChangeEvent.isFull = isFull;
})(TextDocumentContentChangeEvent = exports.TextDocumentContentChangeEvent || (exports.TextDocumentContentChangeEvent = {}));
/**
 * The document change notification is sent from the client to the server to signal
 * changes to a text document.
 */
var DidChangeTextDocumentNotification;
(function (DidChangeTextDocumentNotification) {
    DidChangeTextDocumentNotification.method = 'textDocument/didChange';
    DidChangeTextDocumentNotification.type = new messages_1.ProtocolNotificationType(DidChangeTextDocumentNotification.method);
})(DidChangeTextDocumentNotification = exports.DidChangeTextDocumentNotification || (exports.DidChangeTextDocumentNotification = {}));
/**
 * The document close notification is sent from the client to the server when
 * the document got closed in the client. The document's truth now exists where
 * the document's uri points to (e.g. if the document's uri is a file uri the
 * truth now exists on disk). As with the open notification the close notification
 * is about managing the document's content. Receiving a close notification
 * doesn't mean that the document was open in an editor before. A close
 * notification requires a previous open notification to be sent.
 */
var DidCloseTextDocumentNotification;
(function (DidCloseTextDocumentNotification) {
    DidCloseTextDocumentNotification.method = 'textDocument/didClose';
    DidCloseTextDocumentNotification.type = new messages_1.ProtocolNotificationType(DidCloseTextDocumentNotification.method);
})(DidCloseTextDocumentNotification = exports.DidCloseTextDocumentNotification || (exports.DidCloseTextDocumentNotification = {}));
/**
 * The document save notification is sent from the client to the server when
 * the document got saved in the client.
 */
var DidSaveTextDocumentNotification;
(function (DidSaveTextDocumentNotification) {
    DidSaveTextDocumentNotification.method = 'textDocument/didSave';
    DidSaveTextDocumentNotification.type = new messages_1.ProtocolNotificationType(DidSaveTextDocumentNotification.method);
})(DidSaveTextDocumentNotification = exports.DidSaveTextDocumentNotification || (exports.DidSaveTextDocumentNotification = {}));
/**
 * Represents reasons why a text document is saved.
 */
var TextDocumentSaveReason;
(function (TextDocumentSaveReason) {
    /**
     * Manually triggered, e.g. by the user pressing save, by starting debugging,
     * or by an API call.
     */
    TextDocumentSaveReason.Manual = 1;
    /**
     * Automatic after a delay.
     */
    TextDocumentSaveReason.AfterDelay = 2;
    /**
     * When the editor lost focus.
     */
    TextDocumentSaveReason.FocusOut = 3;
})(TextDocumentSaveReason = exports.TextDocumentSaveReason || (exports.TextDocumentSaveReason = {}));
/**
 * A document will save notification is sent from the client to the server before
 * the document is actually saved.
 */
var WillSaveTextDocumentNotification;
(function (WillSaveTextDocumentNotification) {
    WillSaveTextDocumentNotification.method = 'textDocument/willSave';
    WillSaveTextDocumentNotification.type = new messages_1.ProtocolNotificationType(WillSaveTextDocumentNotification.method);
})(WillSaveTextDocumentNotification = exports.WillSaveTextDocumentNotification || (exports.WillSaveTextDocumentNotification = {}));
/**
 * A document will save request is sent from the client to the server before
 * the document is actually saved. The request can return an array of TextEdits
 * which will be applied to the text document before it is saved. Please note that
 * clients might drop results if computing the text edits took too long or if a
 * server constantly fails on this request. This is done to keep the save fast and
 * reliable.
 */
var WillSaveTextDocumentWaitUntilRequest;
(function (WillSaveTextDocumentWaitUntilRequest) {
    WillSaveTextDocumentWaitUntilRequest.method = 'textDocument/willSaveWaitUntil';
    WillSaveTextDocumentWaitUntilRequest.type = new messages_1.ProtocolRequestType(WillSaveTextDocumentWaitUntilRequest.method);
})(WillSaveTextDocumentWaitUntilRequest = exports.WillSaveTextDocumentWaitUntilRequest || (exports.WillSaveTextDocumentWaitUntilRequest = {}));
/**
 * The watched files notification is sent from the client to the server when
 * the client detects changes to file watched by the language client.
 */
var DidChangeWatchedFilesNotification;
(function (DidChangeWatchedFilesNotification) {
    DidChangeWatchedFilesNotification.type = new messages_1.ProtocolNotificationType('workspace/didChangeWatchedFiles');
})(DidChangeWatchedFilesNotification = exports.DidChangeWatchedFilesNotification || (exports.DidChangeWatchedFilesNotification = {}));
/**
 * The file event type
 */
var FileChangeType;
(function (FileChangeType) {
    /**
     * The file got created.
     */
    FileChangeType.Created = 1;
    /**
     * The file got changed.
     */
    FileChangeType.Changed = 2;
    /**
     * The file got deleted.
     */
    FileChangeType.Deleted = 3;
})(FileChangeType = exports.FileChangeType || (exports.FileChangeType = {}));
var WatchKind;
(function (WatchKind) {
    /**
     * Interested in create events.
     */
    WatchKind.Create = 1;
    /**
     * Interested in change events
     */
    WatchKind.Change = 2;
    /**
     * Interested in delete events
     */
    WatchKind.Delete = 4;
})(WatchKind = exports.WatchKind || (exports.WatchKind = {}));
/**
 * Diagnostics notification are sent from the server to the client to signal
 * results of validation runs.
 */
var PublishDiagnosticsNotification;
(function (PublishDiagnosticsNotification) {
    PublishDiagnosticsNotification.type = new messages_1.ProtocolNotificationType('textDocument/publishDiagnostics');
})(PublishDiagnosticsNotification = exports.PublishDiagnosticsNotification || (exports.PublishDiagnosticsNotification = {}));
/**
 * How a completion was triggered
 */
var CompletionTriggerKind;
(function (CompletionTriggerKind) {
    /**
     * Completion was triggered by typing an identifier (24x7 code
     * complete), manual invocation (e.g Ctrl+Space) or via API.
     */
    CompletionTriggerKind.Invoked = 1;
    /**
     * Completion was triggered by a trigger character specified by
     * the `triggerCharacters` properties of the `CompletionRegistrationOptions`.
     */
    CompletionTriggerKind.TriggerCharacter = 2;
    /**
     * Completion was re-triggered as current completion list is incomplete
     */
    CompletionTriggerKind.TriggerForIncompleteCompletions = 3;
})(CompletionTriggerKind = exports.CompletionTriggerKind || (exports.CompletionTriggerKind = {}));
/**
 * Request to request completion at a given text document position. The request's
 * parameter is of type [TextDocumentPosition](#TextDocumentPosition) the response
 * is of type [CompletionItem[]](#CompletionItem) or [CompletionList](#CompletionList)
 * or a Thenable that resolves to such.
 *
 * The request can delay the computation of the [`detail`](#CompletionItem.detail)
 * and [`documentation`](#CompletionItem.documentation) properties to the `completionItem/resolve`
 * request. However, properties that are needed for the initial sorting and filtering, like `sortText`,
 * `filterText`, `insertText`, and `textEdit`, must not be changed during resolve.
 */
var CompletionRequest;
(function (CompletionRequest) {
    CompletionRequest.method = 'textDocument/completion';
    CompletionRequest.type = new messages_1.ProtocolRequestType(CompletionRequest.method);
})(CompletionRequest = exports.CompletionRequest || (exports.CompletionRequest = {}));
/**
 * Request to resolve additional information for a given completion item.The request's
 * parameter is of type [CompletionItem](#CompletionItem) the response
 * is of type [CompletionItem](#CompletionItem) or a Thenable that resolves to such.
 */
var CompletionResolveRequest;
(function (CompletionResolveRequest) {
    CompletionResolveRequest.method = 'completionItem/resolve';
    CompletionResolveRequest.type = new messages_1.ProtocolRequestType(CompletionResolveRequest.method);
})(CompletionResolveRequest = exports.CompletionResolveRequest || (exports.CompletionResolveRequest = {}));
/**
 * Request to request hover information at a given text document position. The request's
 * parameter is of type [TextDocumentPosition](#TextDocumentPosition) the response is of
 * type [Hover](#Hover) or a Thenable that resolves to such.
 */
var HoverRequest;
(function (HoverRequest) {
    HoverRequest.method = 'textDocument/hover';
    HoverRequest.type = new messages_1.ProtocolRequestType(HoverRequest.method);
})(HoverRequest = exports.HoverRequest || (exports.HoverRequest = {}));
/**
 * How a signature help was triggered.
 *
 * @since 3.15.0
 */
var SignatureHelpTriggerKind;
(function (SignatureHelpTriggerKind) {
    /**
     * Signature help was invoked manually by the user or by a command.
     */
    SignatureHelpTriggerKind.Invoked = 1;
    /**
     * Signature help was triggered by a trigger character.
     */
    SignatureHelpTriggerKind.TriggerCharacter = 2;
    /**
     * Signature help was triggered by the cursor moving or by the document content changing.
     */
    SignatureHelpTriggerKind.ContentChange = 3;
})(SignatureHelpTriggerKind = exports.SignatureHelpTriggerKind || (exports.SignatureHelpTriggerKind = {}));
var SignatureHelpRequest;
(function (SignatureHelpRequest) {
    SignatureHelpRequest.method = 'textDocument/signatureHelp';
    SignatureHelpRequest.type = new messages_1.ProtocolRequestType(SignatureHelpRequest.method);
})(SignatureHelpRequest = exports.SignatureHelpRequest || (exports.SignatureHelpRequest = {}));
/**
 * A request to resolve the definition location of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPosition]
 * (#TextDocumentPosition) the response is of either type [Definition](#Definition)
 * or a typed array of [DefinitionLink](#DefinitionLink) or a Thenable that resolves
 * to such.
 */
var DefinitionRequest;
(function (DefinitionRequest) {
    DefinitionRequest.method = 'textDocument/definition';
    DefinitionRequest.type = new messages_1.ProtocolRequestType(DefinitionRequest.method);
})(DefinitionRequest = exports.DefinitionRequest || (exports.DefinitionRequest = {}));
/**
 * A request to resolve project-wide references for the symbol denoted
 * by the given text document position. The request's parameter is of
 * type [ReferenceParams](#ReferenceParams) the response is of type
 * [Location[]](#Location) or a Thenable that resolves to such.
 */
var ReferencesRequest;
(function (ReferencesRequest) {
    ReferencesRequest.method = 'textDocument/references';
    ReferencesRequest.type = new messages_1.ProtocolRequestType(ReferencesRequest.method);
})(ReferencesRequest = exports.ReferencesRequest || (exports.ReferencesRequest = {}));
/**
 * Request to resolve a [DocumentHighlight](#DocumentHighlight) for a given
 * text document position. The request's parameter is of type [TextDocumentPosition]
 * (#TextDocumentPosition) the request response is of type [DocumentHighlight[]]
 * (#DocumentHighlight) or a Thenable that resolves to such.
 */
var DocumentHighlightRequest;
(function (DocumentHighlightRequest) {
    DocumentHighlightRequest.method = 'textDocument/documentHighlight';
    DocumentHighlightRequest.type = new messages_1.ProtocolRequestType(DocumentHighlightRequest.method);
})(DocumentHighlightRequest = exports.DocumentHighlightRequest || (exports.DocumentHighlightRequest = {}));
/**
 * A request to list all symbols found in a given text document. The request's
 * parameter is of type [TextDocumentIdentifier](#TextDocumentIdentifier) the
 * response is of type [SymbolInformation[]](#SymbolInformation) or a Thenable
 * that resolves to such.
 */
var DocumentSymbolRequest;
(function (DocumentSymbolRequest) {
    DocumentSymbolRequest.method = 'textDocument/documentSymbol';
    DocumentSymbolRequest.type = new messages_1.ProtocolRequestType(DocumentSymbolRequest.method);
})(DocumentSymbolRequest = exports.DocumentSymbolRequest || (exports.DocumentSymbolRequest = {}));
/**
 * A request to provide commands for the given text document and range.
 */
var CodeActionRequest;
(function (CodeActionRequest) {
    CodeActionRequest.method = 'textDocument/codeAction';
    CodeActionRequest.type = new messages_1.ProtocolRequestType(CodeActionRequest.method);
})(CodeActionRequest = exports.CodeActionRequest || (exports.CodeActionRequest = {}));
/**
 * Request to resolve additional information for a given code action.The request's
 * parameter is of type [CodeAction](#CodeAction) the response
 * is of type [CodeAction](#CodeAction) or a Thenable that resolves to such.
 */
var CodeActionResolveRequest;
(function (CodeActionResolveRequest) {
    CodeActionResolveRequest.method = 'codeAction/resolve';
    CodeActionResolveRequest.type = new messages_1.ProtocolRequestType(CodeActionResolveRequest.method);
})(CodeActionResolveRequest = exports.CodeActionResolveRequest || (exports.CodeActionResolveRequest = {}));
/**
 * A request to list project-wide symbols matching the query string given
 * by the [WorkspaceSymbolParams](#WorkspaceSymbolParams). The response is
 * of type [SymbolInformation[]](#SymbolInformation) or a Thenable that
 * resolves to such.
 */
var WorkspaceSymbolRequest;
(function (WorkspaceSymbolRequest) {
    WorkspaceSymbolRequest.method = 'workspace/symbol';
    WorkspaceSymbolRequest.type = new messages_1.ProtocolRequestType(WorkspaceSymbolRequest.method);
})(WorkspaceSymbolRequest = exports.WorkspaceSymbolRequest || (exports.WorkspaceSymbolRequest = {}));
/**
 * A request to provide code lens for the given text document.
 */
var CodeLensRequest;
(function (CodeLensRequest) {
    CodeLensRequest.method = 'textDocument/codeLens';
    CodeLensRequest.type = new messages_1.ProtocolRequestType(CodeLensRequest.method);
})(CodeLensRequest = exports.CodeLensRequest || (exports.CodeLensRequest = {}));
/**
 * A request to resolve a command for a given code lens.
 */
var CodeLensResolveRequest;
(function (CodeLensResolveRequest) {
    CodeLensResolveRequest.method = 'codeLens/resolve';
    CodeLensResolveRequest.type = new messages_1.ProtocolRequestType(CodeLensResolveRequest.method);
})(CodeLensResolveRequest = exports.CodeLensResolveRequest || (exports.CodeLensResolveRequest = {}));
/**
 * A request to refresh all code actions
 *
 * @since 3.16.0
 */
var CodeLensRefreshRequest;
(function (CodeLensRefreshRequest) {
    CodeLensRefreshRequest.method = `workspace/codeLens/refresh`;
    CodeLensRefreshRequest.type = new messages_1.ProtocolRequestType0(CodeLensRefreshRequest.method);
})(CodeLensRefreshRequest = exports.CodeLensRefreshRequest || (exports.CodeLensRefreshRequest = {}));
/**
 * A request to provide document links
 */
var DocumentLinkRequest;
(function (DocumentLinkRequest) {
    DocumentLinkRequest.method = 'textDocument/documentLink';
    DocumentLinkRequest.type = new messages_1.ProtocolRequestType(DocumentLinkRequest.method);
})(DocumentLinkRequest = exports.DocumentLinkRequest || (exports.DocumentLinkRequest = {}));
/**
 * Request to resolve additional information for a given document link. The request's
 * parameter is of type [DocumentLink](#DocumentLink) the response
 * is of type [DocumentLink](#DocumentLink) or a Thenable that resolves to such.
 */
var DocumentLinkResolveRequest;
(function (DocumentLinkResolveRequest) {
    DocumentLinkResolveRequest.method = 'documentLink/resolve';
    DocumentLinkResolveRequest.type = new messages_1.ProtocolRequestType(DocumentLinkResolveRequest.method);
})(DocumentLinkResolveRequest = exports.DocumentLinkResolveRequest || (exports.DocumentLinkResolveRequest = {}));
/**
 * A request to to format a whole document.
 */
var DocumentFormattingRequest;
(function (DocumentFormattingRequest) {
    DocumentFormattingRequest.method = 'textDocument/formatting';
    DocumentFormattingRequest.type = new messages_1.ProtocolRequestType(DocumentFormattingRequest.method);
})(DocumentFormattingRequest = exports.DocumentFormattingRequest || (exports.DocumentFormattingRequest = {}));
/**
 * A request to to format a range in a document.
 */
var DocumentRangeFormattingRequest;
(function (DocumentRangeFormattingRequest) {
    DocumentRangeFormattingRequest.method = 'textDocument/rangeFormatting';
    DocumentRangeFormattingRequest.type = new messages_1.ProtocolRequestType(DocumentRangeFormattingRequest.method);
})(DocumentRangeFormattingRequest = exports.DocumentRangeFormattingRequest || (exports.DocumentRangeFormattingRequest = {}));
/**
 * A request to format a document on type.
 */
var DocumentOnTypeFormattingRequest;
(function (DocumentOnTypeFormattingRequest) {
    DocumentOnTypeFormattingRequest.method = 'textDocument/onTypeFormatting';
    DocumentOnTypeFormattingRequest.type = new messages_1.ProtocolRequestType(DocumentOnTypeFormattingRequest.method);
})(DocumentOnTypeFormattingRequest = exports.DocumentOnTypeFormattingRequest || (exports.DocumentOnTypeFormattingRequest = {}));
//---- Rename ----------------------------------------------
var PrepareSupportDefaultBehavior;
(function (PrepareSupportDefaultBehavior) {
    /**
     * The client's default behavior is to select the identifier
     * according the to language's syntax rule.
     */
    PrepareSupportDefaultBehavior.Identifier = 1;
})(PrepareSupportDefaultBehavior = exports.PrepareSupportDefaultBehavior || (exports.PrepareSupportDefaultBehavior = {}));
/**
 * A request to rename a symbol.
 */
var RenameRequest;
(function (RenameRequest) {
    RenameRequest.method = 'textDocument/rename';
    RenameRequest.type = new messages_1.ProtocolRequestType(RenameRequest.method);
})(RenameRequest = exports.RenameRequest || (exports.RenameRequest = {}));
/**
 * A request to test and perform the setup necessary for a rename.
 *
 * @since 3.16 - support for default behavior
 */
var PrepareRenameRequest;
(function (PrepareRenameRequest) {
    PrepareRenameRequest.method = 'textDocument/prepareRename';
    PrepareRenameRequest.type = new messages_1.ProtocolRequestType(PrepareRenameRequest.method);
})(PrepareRenameRequest = exports.PrepareRenameRequest || (exports.PrepareRenameRequest = {}));
/**
 * A request send from the client to the server to execute a command. The request might return
 * a workspace edit which the client will apply to the workspace.
 */
var ExecuteCommandRequest;
(function (ExecuteCommandRequest) {
    ExecuteCommandRequest.type = new messages_1.ProtocolRequestType('workspace/executeCommand');
})(ExecuteCommandRequest = exports.ExecuteCommandRequest || (exports.ExecuteCommandRequest = {}));
/**
 * A request sent from the server to the client to modified certain resources.
 */
var ApplyWorkspaceEditRequest;
(function (ApplyWorkspaceEditRequest) {
    ApplyWorkspaceEditRequest.type = new messages_1.ProtocolRequestType('workspace/applyEdit');
})(ApplyWorkspaceEditRequest = exports.ApplyWorkspaceEditRequest || (exports.ApplyWorkspaceEditRequest = {}));
//# sourceMappingURL=protocol.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.linkedEditingRange.js":
/*!**********************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.linkedEditingRange.js ***!
  \**********************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.LinkedEditingRangeRequest = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
/**
 * A request to provide ranges that can be edited together.
 *
 * @since 3.16.0
 */
var LinkedEditingRangeRequest;
(function (LinkedEditingRangeRequest) {
    LinkedEditingRangeRequest.method = 'textDocument/linkedEditingRange';
    LinkedEditingRangeRequest.type = new messages_1.ProtocolRequestType(LinkedEditingRangeRequest.method);
})(LinkedEditingRangeRequest = exports.LinkedEditingRangeRequest || (exports.LinkedEditingRangeRequest = {}));
//# sourceMappingURL=protocol.linkedEditingRange.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.moniker.js":
/*!***********************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.moniker.js ***!
  \***********************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.MonikerRequest = exports.MonikerKind = exports.UniquenessLevel = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
/**
 * Moniker uniqueness level to define scope of the moniker.
 *
 * @since 3.16.0
 */
var UniquenessLevel;
(function (UniquenessLevel) {
    /**
     * The moniker is only unique inside a document
     */
    UniquenessLevel["document"] = "document";
    /**
     * The moniker is unique inside a project for which a dump got created
     */
    UniquenessLevel["project"] = "project";
    /**
     * The moniker is unique inside the group to which a project belongs
     */
    UniquenessLevel["group"] = "group";
    /**
     * The moniker is unique inside the moniker scheme.
     */
    UniquenessLevel["scheme"] = "scheme";
    /**
     * The moniker is globally unique
     */
    UniquenessLevel["global"] = "global";
})(UniquenessLevel = exports.UniquenessLevel || (exports.UniquenessLevel = {}));
/**
 * The moniker kind.
 *
 * @since 3.16.0
 */
var MonikerKind;
(function (MonikerKind) {
    /**
     * The moniker represent a symbol that is imported into a project
     */
    MonikerKind["import"] = "import";
    /**
     * The moniker represents a symbol that is exported from a project
     */
    MonikerKind["export"] = "export";
    /**
     * The moniker represents a symbol that is local to a project (e.g. a local
     * variable of a function, a class not visible outside the project, ...)
     */
    MonikerKind["local"] = "local";
})(MonikerKind = exports.MonikerKind || (exports.MonikerKind = {}));
/**
 * A request to get the moniker of a symbol at a given text document position.
 * The request parameter is of type [TextDocumentPositionParams](#TextDocumentPositionParams).
 * The response is of type [Moniker[]](#Moniker[]) or `null`.
 */
var MonikerRequest;
(function (MonikerRequest) {
    MonikerRequest.method = 'textDocument/moniker';
    MonikerRequest.type = new messages_1.ProtocolRequestType(MonikerRequest.method);
})(MonikerRequest = exports.MonikerRequest || (exports.MonikerRequest = {}));
//# sourceMappingURL=protocol.moniker.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.progress.js":
/*!************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.progress.js ***!
  \************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WorkDoneProgressCancelNotification = exports.WorkDoneProgressCreateRequest = exports.WorkDoneProgress = void 0;
const vscode_jsonrpc_1 = __webpack_require__(/*! vscode-jsonrpc */ "./src/lang-server/client/node_modules/vscode-jsonrpc/lib/browser/main.js");
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
var WorkDoneProgress;
(function (WorkDoneProgress) {
    WorkDoneProgress.type = new vscode_jsonrpc_1.ProgressType();
    function is(value) {
        return value === WorkDoneProgress.type;
    }
    WorkDoneProgress.is = is;
})(WorkDoneProgress = exports.WorkDoneProgress || (exports.WorkDoneProgress = {}));
/**
 * The `window/workDoneProgress/create` request is sent from the server to the client to initiate progress
 * reporting from the server.
 */
var WorkDoneProgressCreateRequest;
(function (WorkDoneProgressCreateRequest) {
    WorkDoneProgressCreateRequest.type = new messages_1.ProtocolRequestType('window/workDoneProgress/create');
})(WorkDoneProgressCreateRequest = exports.WorkDoneProgressCreateRequest || (exports.WorkDoneProgressCreateRequest = {}));
/**
 * The `window/workDoneProgress/cancel` notification is sent from  the client to the server to cancel a progress
 * initiated on the server side.
 */
var WorkDoneProgressCancelNotification;
(function (WorkDoneProgressCancelNotification) {
    WorkDoneProgressCancelNotification.type = new messages_1.ProtocolNotificationType('window/workDoneProgress/cancel');
})(WorkDoneProgressCancelNotification = exports.WorkDoneProgressCancelNotification || (exports.WorkDoneProgressCancelNotification = {}));
//# sourceMappingURL=protocol.progress.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.selectionRange.js":
/*!******************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.selectionRange.js ***!
  \******************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SelectionRangeRequest = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
/**
 * A request to provide selection ranges in a document. The request's
 * parameter is of type [SelectionRangeParams](#SelectionRangeParams), the
 * response is of type [SelectionRange[]](#SelectionRange[]) or a Thenable
 * that resolves to such.
 */
var SelectionRangeRequest;
(function (SelectionRangeRequest) {
    SelectionRangeRequest.method = 'textDocument/selectionRange';
    SelectionRangeRequest.type = new messages_1.ProtocolRequestType(SelectionRangeRequest.method);
})(SelectionRangeRequest = exports.SelectionRangeRequest || (exports.SelectionRangeRequest = {}));
//# sourceMappingURL=protocol.selectionRange.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.semanticTokens.js":
/*!******************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.semanticTokens.js ***!
  \******************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SemanticTokensRefreshRequest = exports.SemanticTokensRangeRequest = exports.SemanticTokensDeltaRequest = exports.SemanticTokensRequest = exports.SemanticTokensRegistrationType = exports.TokenFormat = exports.SemanticTokens = exports.SemanticTokenModifiers = exports.SemanticTokenTypes = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
/**
 * A set of predefined token types. This set is not fixed
 * an clients can specify additional token types via the
 * corresponding client capabilities.
 *
 * @since 3.16.0
 */
var SemanticTokenTypes;
(function (SemanticTokenTypes) {
    SemanticTokenTypes["namespace"] = "namespace";
    /**
     * Represents a generic type. Acts as a fallback for types which can't be mapped to
     * a specific type like class or enum.
     */
    SemanticTokenTypes["type"] = "type";
    SemanticTokenTypes["class"] = "class";
    SemanticTokenTypes["enum"] = "enum";
    SemanticTokenTypes["interface"] = "interface";
    SemanticTokenTypes["struct"] = "struct";
    SemanticTokenTypes["typeParameter"] = "typeParameter";
    SemanticTokenTypes["parameter"] = "parameter";
    SemanticTokenTypes["variable"] = "variable";
    SemanticTokenTypes["property"] = "property";
    SemanticTokenTypes["enumMember"] = "enumMember";
    SemanticTokenTypes["event"] = "event";
    SemanticTokenTypes["function"] = "function";
    SemanticTokenTypes["method"] = "method";
    SemanticTokenTypes["macro"] = "macro";
    SemanticTokenTypes["keyword"] = "keyword";
    SemanticTokenTypes["modifier"] = "modifier";
    SemanticTokenTypes["comment"] = "comment";
    SemanticTokenTypes["string"] = "string";
    SemanticTokenTypes["number"] = "number";
    SemanticTokenTypes["regexp"] = "regexp";
    SemanticTokenTypes["operator"] = "operator";
})(SemanticTokenTypes = exports.SemanticTokenTypes || (exports.SemanticTokenTypes = {}));
/**
 * A set of predefined token modifiers. This set is not fixed
 * an clients can specify additional token types via the
 * corresponding client capabilities.
 *
 * @since 3.16.0
 */
var SemanticTokenModifiers;
(function (SemanticTokenModifiers) {
    SemanticTokenModifiers["declaration"] = "declaration";
    SemanticTokenModifiers["definition"] = "definition";
    SemanticTokenModifiers["readonly"] = "readonly";
    SemanticTokenModifiers["static"] = "static";
    SemanticTokenModifiers["deprecated"] = "deprecated";
    SemanticTokenModifiers["abstract"] = "abstract";
    SemanticTokenModifiers["async"] = "async";
    SemanticTokenModifiers["modification"] = "modification";
    SemanticTokenModifiers["documentation"] = "documentation";
    SemanticTokenModifiers["defaultLibrary"] = "defaultLibrary";
})(SemanticTokenModifiers = exports.SemanticTokenModifiers || (exports.SemanticTokenModifiers = {}));
/**
 * @since 3.16.0
 */
var SemanticTokens;
(function (SemanticTokens) {
    function is(value) {
        const candidate = value;
        return candidate !== undefined && (candidate.resultId === undefined || typeof candidate.resultId === 'string') &&
            Array.isArray(candidate.data) && (candidate.data.length === 0 || typeof candidate.data[0] === 'number');
    }
    SemanticTokens.is = is;
})(SemanticTokens = exports.SemanticTokens || (exports.SemanticTokens = {}));
//------- 'textDocument/semanticTokens' -----
var TokenFormat;
(function (TokenFormat) {
    TokenFormat.Relative = 'relative';
})(TokenFormat = exports.TokenFormat || (exports.TokenFormat = {}));
var SemanticTokensRegistrationType;
(function (SemanticTokensRegistrationType) {
    SemanticTokensRegistrationType.method = 'textDocument/semanticTokens';
    SemanticTokensRegistrationType.type = new messages_1.RegistrationType(SemanticTokensRegistrationType.method);
})(SemanticTokensRegistrationType = exports.SemanticTokensRegistrationType || (exports.SemanticTokensRegistrationType = {}));
/**
 * @since 3.16.0
 */
var SemanticTokensRequest;
(function (SemanticTokensRequest) {
    SemanticTokensRequest.method = 'textDocument/semanticTokens/full';
    SemanticTokensRequest.type = new messages_1.ProtocolRequestType(SemanticTokensRequest.method);
})(SemanticTokensRequest = exports.SemanticTokensRequest || (exports.SemanticTokensRequest = {}));
/**
 * @since 3.16.0
 */
var SemanticTokensDeltaRequest;
(function (SemanticTokensDeltaRequest) {
    SemanticTokensDeltaRequest.method = 'textDocument/semanticTokens/full/delta';
    SemanticTokensDeltaRequest.type = new messages_1.ProtocolRequestType(SemanticTokensDeltaRequest.method);
})(SemanticTokensDeltaRequest = exports.SemanticTokensDeltaRequest || (exports.SemanticTokensDeltaRequest = {}));
/**
 * @since 3.16.0
 */
var SemanticTokensRangeRequest;
(function (SemanticTokensRangeRequest) {
    SemanticTokensRangeRequest.method = 'textDocument/semanticTokens/range';
    SemanticTokensRangeRequest.type = new messages_1.ProtocolRequestType(SemanticTokensRangeRequest.method);
})(SemanticTokensRangeRequest = exports.SemanticTokensRangeRequest || (exports.SemanticTokensRangeRequest = {}));
/**
 * @since 3.16.0
 */
var SemanticTokensRefreshRequest;
(function (SemanticTokensRefreshRequest) {
    SemanticTokensRefreshRequest.method = `workspace/semanticTokens/refresh`;
    SemanticTokensRefreshRequest.type = new messages_1.ProtocolRequestType0(SemanticTokensRefreshRequest.method);
})(SemanticTokensRefreshRequest = exports.SemanticTokensRefreshRequest || (exports.SemanticTokensRefreshRequest = {}));
//# sourceMappingURL=protocol.semanticTokens.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.showDocument.js":
/*!****************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.showDocument.js ***!
  \****************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ShowDocumentRequest = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
/**
 * A request to show a document. This request might open an
 * external program depending on the value of the URI to open.
 * For example a request to open `https://code.visualstudio.com/`
 * will very likely open the URI in a WEB browser.
 *
 * @since 3.16.0
*/
var ShowDocumentRequest;
(function (ShowDocumentRequest) {
    ShowDocumentRequest.method = 'window/showDocument';
    ShowDocumentRequest.type = new messages_1.ProtocolRequestType(ShowDocumentRequest.method);
})(ShowDocumentRequest = exports.ShowDocumentRequest || (exports.ShowDocumentRequest = {}));
//# sourceMappingURL=protocol.showDocument.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.typeDefinition.js":
/*!******************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.typeDefinition.js ***!
  \******************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.TypeDefinitionRequest = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
// @ts-ignore: to avoid inlining LocatioLink as dynamic import
let __noDynamicImport;
/**
 * A request to resolve the type definition locations of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPositioParams]
 * (#TextDocumentPositionParams) the response is of type [Definition](#Definition) or a
 * Thenable that resolves to such.
 */
var TypeDefinitionRequest;
(function (TypeDefinitionRequest) {
    TypeDefinitionRequest.method = 'textDocument/typeDefinition';
    TypeDefinitionRequest.type = new messages_1.ProtocolRequestType(TypeDefinitionRequest.method);
})(TypeDefinitionRequest = exports.TypeDefinitionRequest || (exports.TypeDefinitionRequest = {}));
//# sourceMappingURL=protocol.typeDefinition.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.workspaceFolders.js":
/*!********************************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/protocol.workspaceFolders.js ***!
  \********************************************************************************************************************/
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DidChangeWorkspaceFoldersNotification = exports.WorkspaceFoldersRequest = void 0;
const messages_1 = __webpack_require__(/*! ./messages */ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/messages.js");
/**
 * The `workspace/workspaceFolders` is sent from the server to the client to fetch the open workspace folders.
 */
var WorkspaceFoldersRequest;
(function (WorkspaceFoldersRequest) {
    WorkspaceFoldersRequest.type = new messages_1.ProtocolRequestType0('workspace/workspaceFolders');
})(WorkspaceFoldersRequest = exports.WorkspaceFoldersRequest || (exports.WorkspaceFoldersRequest = {}));
/**
 * The `workspace/didChangeWorkspaceFolders` notification is sent from the client to the server when the workspace
 * folder configuration changes.
 */
var DidChangeWorkspaceFoldersNotification;
(function (DidChangeWorkspaceFoldersNotification) {
    DidChangeWorkspaceFoldersNotification.type = new messages_1.ProtocolNotificationType('workspace/didChangeWorkspaceFolders');
})(DidChangeWorkspaceFoldersNotification = exports.DidChangeWorkspaceFoldersNotification || (exports.DidChangeWorkspaceFoldersNotification = {}));
//# sourceMappingURL=protocol.workspaceFolders.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/utils/is.js":
/*!***************************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-protocol/lib/common/utils/is.js ***!
  \***************************************************************************************************/
/***/ ((__unused_webpack_module, exports) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.objectLiteral = exports.typedArray = exports.stringArray = exports.array = exports.func = exports.error = exports.number = exports.string = exports.boolean = void 0;
function boolean(value) {
    return value === true || value === false;
}
exports.boolean = boolean;
function string(value) {
    return typeof value === 'string' || value instanceof String;
}
exports.string = string;
function number(value) {
    return typeof value === 'number' || value instanceof Number;
}
exports.number = number;
function error(value) {
    return value instanceof Error;
}
exports.error = error;
function func(value) {
    return typeof value === 'function';
}
exports.func = func;
function array(value) {
    return Array.isArray(value);
}
exports.array = array;
function stringArray(value) {
    return array(value) && value.every(elem => string(elem));
}
exports.stringArray = stringArray;
function typedArray(value, check) {
    return Array.isArray(value) && value.every(check);
}
exports.typedArray = typedArray;
function objectLiteral(value) {
    // Strictly speaking class instances pass this check as well. Since the LSP
    // doesn't use classes we ignore this for now. If we do we need to add something
    // like this: `Object.getPrototypeOf(Object.getPrototypeOf(x)) === null`
    return value !== null && typeof value === 'object';
}
exports.objectLiteral = objectLiteral;
//# sourceMappingURL=is.js.map

/***/ }),

/***/ "./src/lang-server/client/node_modules/vscode-languageserver-types/lib/esm/main.js":
/*!*****************************************************************************************!*\
  !*** ./src/lang-server/client/node_modules/vscode-languageserver-types/lib/esm/main.js ***!
  \*****************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "integer": () => (/* binding */ integer),
/* harmony export */   "uinteger": () => (/* binding */ uinteger),
/* harmony export */   "Position": () => (/* binding */ Position),
/* harmony export */   "Range": () => (/* binding */ Range),
/* harmony export */   "Location": () => (/* binding */ Location),
/* harmony export */   "LocationLink": () => (/* binding */ LocationLink),
/* harmony export */   "Color": () => (/* binding */ Color),
/* harmony export */   "ColorInformation": () => (/* binding */ ColorInformation),
/* harmony export */   "ColorPresentation": () => (/* binding */ ColorPresentation),
/* harmony export */   "FoldingRangeKind": () => (/* binding */ FoldingRangeKind),
/* harmony export */   "FoldingRange": () => (/* binding */ FoldingRange),
/* harmony export */   "DiagnosticRelatedInformation": () => (/* binding */ DiagnosticRelatedInformation),
/* harmony export */   "DiagnosticSeverity": () => (/* binding */ DiagnosticSeverity),
/* harmony export */   "DiagnosticTag": () => (/* binding */ DiagnosticTag),
/* harmony export */   "CodeDescription": () => (/* binding */ CodeDescription),
/* harmony export */   "Diagnostic": () => (/* binding */ Diagnostic),
/* harmony export */   "Command": () => (/* binding */ Command),
/* harmony export */   "TextEdit": () => (/* binding */ TextEdit),
/* harmony export */   "ChangeAnnotation": () => (/* binding */ ChangeAnnotation),
/* harmony export */   "ChangeAnnotationIdentifier": () => (/* binding */ ChangeAnnotationIdentifier),
/* harmony export */   "AnnotatedTextEdit": () => (/* binding */ AnnotatedTextEdit),
/* harmony export */   "TextDocumentEdit": () => (/* binding */ TextDocumentEdit),
/* harmony export */   "CreateFile": () => (/* binding */ CreateFile),
/* harmony export */   "RenameFile": () => (/* binding */ RenameFile),
/* harmony export */   "DeleteFile": () => (/* binding */ DeleteFile),
/* harmony export */   "WorkspaceEdit": () => (/* binding */ WorkspaceEdit),
/* harmony export */   "WorkspaceChange": () => (/* binding */ WorkspaceChange),
/* harmony export */   "TextDocumentIdentifier": () => (/* binding */ TextDocumentIdentifier),
/* harmony export */   "VersionedTextDocumentIdentifier": () => (/* binding */ VersionedTextDocumentIdentifier),
/* harmony export */   "OptionalVersionedTextDocumentIdentifier": () => (/* binding */ OptionalVersionedTextDocumentIdentifier),
/* harmony export */   "TextDocumentItem": () => (/* binding */ TextDocumentItem),
/* harmony export */   "MarkupKind": () => (/* binding */ MarkupKind),
/* harmony export */   "MarkupContent": () => (/* binding */ MarkupContent),
/* harmony export */   "CompletionItemKind": () => (/* binding */ CompletionItemKind),
/* harmony export */   "InsertTextFormat": () => (/* binding */ InsertTextFormat),
/* harmony export */   "CompletionItemTag": () => (/* binding */ CompletionItemTag),
/* harmony export */   "InsertReplaceEdit": () => (/* binding */ InsertReplaceEdit),
/* harmony export */   "InsertTextMode": () => (/* binding */ InsertTextMode),
/* harmony export */   "CompletionItem": () => (/* binding */ CompletionItem),
/* harmony export */   "CompletionList": () => (/* binding */ CompletionList),
/* harmony export */   "MarkedString": () => (/* binding */ MarkedString),
/* harmony export */   "Hover": () => (/* binding */ Hover),
/* harmony export */   "ParameterInformation": () => (/* binding */ ParameterInformation),
/* harmony export */   "SignatureInformation": () => (/* binding */ SignatureInformation),
/* harmony export */   "DocumentHighlightKind": () => (/* binding */ DocumentHighlightKind),
/* harmony export */   "DocumentHighlight": () => (/* binding */ DocumentHighlight),
/* harmony export */   "SymbolKind": () => (/* binding */ SymbolKind),
/* harmony export */   "SymbolTag": () => (/* binding */ SymbolTag),
/* harmony export */   "SymbolInformation": () => (/* binding */ SymbolInformation),
/* harmony export */   "DocumentSymbol": () => (/* binding */ DocumentSymbol),
/* harmony export */   "CodeActionKind": () => (/* binding */ CodeActionKind),
/* harmony export */   "CodeActionContext": () => (/* binding */ CodeActionContext),
/* harmony export */   "CodeAction": () => (/* binding */ CodeAction),
/* harmony export */   "CodeLens": () => (/* binding */ CodeLens),
/* harmony export */   "FormattingOptions": () => (/* binding */ FormattingOptions),
/* harmony export */   "DocumentLink": () => (/* binding */ DocumentLink),
/* harmony export */   "SelectionRange": () => (/* binding */ SelectionRange),
/* harmony export */   "EOL": () => (/* binding */ EOL),
/* harmony export */   "TextDocument": () => (/* binding */ TextDocument)
/* harmony export */ });
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

var integer;
(function (integer) {
    integer.MIN_VALUE = -2147483648;
    integer.MAX_VALUE = 2147483647;
})(integer || (integer = {}));
var uinteger;
(function (uinteger) {
    uinteger.MIN_VALUE = 0;
    uinteger.MAX_VALUE = 2147483647;
})(uinteger || (uinteger = {}));
/**
 * The Position namespace provides helper functions to work with
 * [Position](#Position) literals.
 */
var Position;
(function (Position) {
    /**
     * Creates a new Position literal from the given line and character.
     * @param line The position's line.
     * @param character The position's character.
     */
    function create(line, character) {
        if (line === Number.MAX_VALUE) {
            line = uinteger.MAX_VALUE;
        }
        if (character === Number.MAX_VALUE) {
            character = uinteger.MAX_VALUE;
        }
        return { line: line, character: character };
    }
    Position.create = create;
    /**
     * Checks whether the given literal conforms to the [Position](#Position) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.uinteger(candidate.line) && Is.uinteger(candidate.character);
    }
    Position.is = is;
})(Position || (Position = {}));
/**
 * The Range namespace provides helper functions to work with
 * [Range](#Range) literals.
 */
var Range;
(function (Range) {
    function create(one, two, three, four) {
        if (Is.uinteger(one) && Is.uinteger(two) && Is.uinteger(three) && Is.uinteger(four)) {
            return { start: Position.create(one, two), end: Position.create(three, four) };
        }
        else if (Position.is(one) && Position.is(two)) {
            return { start: one, end: two };
        }
        else {
            throw new Error("Range#create called with invalid arguments[" + one + ", " + two + ", " + three + ", " + four + "]");
        }
    }
    Range.create = create;
    /**
     * Checks whether the given literal conforms to the [Range](#Range) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Position.is(candidate.start) && Position.is(candidate.end);
    }
    Range.is = is;
})(Range || (Range = {}));
/**
 * The Location namespace provides helper functions to work with
 * [Location](#Location) literals.
 */
var Location;
(function (Location) {
    /**
     * Creates a Location literal.
     * @param uri The location's uri.
     * @param range The location's range.
     */
    function create(uri, range) {
        return { uri: uri, range: range };
    }
    Location.create = create;
    /**
     * Checks whether the given literal conforms to the [Location](#Location) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && (Is.string(candidate.uri) || Is.undefined(candidate.uri));
    }
    Location.is = is;
})(Location || (Location = {}));
/**
 * The LocationLink namespace provides helper functions to work with
 * [LocationLink](#LocationLink) literals.
 */
var LocationLink;
(function (LocationLink) {
    /**
     * Creates a LocationLink literal.
     * @param targetUri The definition's uri.
     * @param targetRange The full range of the definition.
     * @param targetSelectionRange The span of the symbol definition at the target.
     * @param originSelectionRange The span of the symbol being defined in the originating source file.
     */
    function create(targetUri, targetRange, targetSelectionRange, originSelectionRange) {
        return { targetUri: targetUri, targetRange: targetRange, targetSelectionRange: targetSelectionRange, originSelectionRange: originSelectionRange };
    }
    LocationLink.create = create;
    /**
     * Checks whether the given literal conforms to the [LocationLink](#LocationLink) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.targetRange) && Is.string(candidate.targetUri)
            && (Range.is(candidate.targetSelectionRange) || Is.undefined(candidate.targetSelectionRange))
            && (Range.is(candidate.originSelectionRange) || Is.undefined(candidate.originSelectionRange));
    }
    LocationLink.is = is;
})(LocationLink || (LocationLink = {}));
/**
 * The Color namespace provides helper functions to work with
 * [Color](#Color) literals.
 */
var Color;
(function (Color) {
    /**
     * Creates a new Color literal.
     */
    function create(red, green, blue, alpha) {
        return {
            red: red,
            green: green,
            blue: blue,
            alpha: alpha,
        };
    }
    Color.create = create;
    /**
     * Checks whether the given literal conforms to the [Color](#Color) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.numberRange(candidate.red, 0, 1)
            && Is.numberRange(candidate.green, 0, 1)
            && Is.numberRange(candidate.blue, 0, 1)
            && Is.numberRange(candidate.alpha, 0, 1);
    }
    Color.is = is;
})(Color || (Color = {}));
/**
 * The ColorInformation namespace provides helper functions to work with
 * [ColorInformation](#ColorInformation) literals.
 */
var ColorInformation;
(function (ColorInformation) {
    /**
     * Creates a new ColorInformation literal.
     */
    function create(range, color) {
        return {
            range: range,
            color: color,
        };
    }
    ColorInformation.create = create;
    /**
     * Checks whether the given literal conforms to the [ColorInformation](#ColorInformation) interface.
     */
    function is(value) {
        var candidate = value;
        return Range.is(candidate.range) && Color.is(candidate.color);
    }
    ColorInformation.is = is;
})(ColorInformation || (ColorInformation = {}));
/**
 * The Color namespace provides helper functions to work with
 * [ColorPresentation](#ColorPresentation) literals.
 */
var ColorPresentation;
(function (ColorPresentation) {
    /**
     * Creates a new ColorInformation literal.
     */
    function create(label, textEdit, additionalTextEdits) {
        return {
            label: label,
            textEdit: textEdit,
            additionalTextEdits: additionalTextEdits,
        };
    }
    ColorPresentation.create = create;
    /**
     * Checks whether the given literal conforms to the [ColorInformation](#ColorInformation) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.string(candidate.label)
            && (Is.undefined(candidate.textEdit) || TextEdit.is(candidate))
            && (Is.undefined(candidate.additionalTextEdits) || Is.typedArray(candidate.additionalTextEdits, TextEdit.is));
    }
    ColorPresentation.is = is;
})(ColorPresentation || (ColorPresentation = {}));
/**
 * Enum of known range kinds
 */
var FoldingRangeKind;
(function (FoldingRangeKind) {
    /**
     * Folding range for a comment
     */
    FoldingRangeKind["Comment"] = "comment";
    /**
     * Folding range for a imports or includes
     */
    FoldingRangeKind["Imports"] = "imports";
    /**
     * Folding range for a region (e.g. `#region`)
     */
    FoldingRangeKind["Region"] = "region";
})(FoldingRangeKind || (FoldingRangeKind = {}));
/**
 * The folding range namespace provides helper functions to work with
 * [FoldingRange](#FoldingRange) literals.
 */
var FoldingRange;
(function (FoldingRange) {
    /**
     * Creates a new FoldingRange literal.
     */
    function create(startLine, endLine, startCharacter, endCharacter, kind) {
        var result = {
            startLine: startLine,
            endLine: endLine
        };
        if (Is.defined(startCharacter)) {
            result.startCharacter = startCharacter;
        }
        if (Is.defined(endCharacter)) {
            result.endCharacter = endCharacter;
        }
        if (Is.defined(kind)) {
            result.kind = kind;
        }
        return result;
    }
    FoldingRange.create = create;
    /**
     * Checks whether the given literal conforms to the [FoldingRange](#FoldingRange) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.uinteger(candidate.startLine) && Is.uinteger(candidate.startLine)
            && (Is.undefined(candidate.startCharacter) || Is.uinteger(candidate.startCharacter))
            && (Is.undefined(candidate.endCharacter) || Is.uinteger(candidate.endCharacter))
            && (Is.undefined(candidate.kind) || Is.string(candidate.kind));
    }
    FoldingRange.is = is;
})(FoldingRange || (FoldingRange = {}));
/**
 * The DiagnosticRelatedInformation namespace provides helper functions to work with
 * [DiagnosticRelatedInformation](#DiagnosticRelatedInformation) literals.
 */
var DiagnosticRelatedInformation;
(function (DiagnosticRelatedInformation) {
    /**
     * Creates a new DiagnosticRelatedInformation literal.
     */
    function create(location, message) {
        return {
            location: location,
            message: message
        };
    }
    DiagnosticRelatedInformation.create = create;
    /**
     * Checks whether the given literal conforms to the [DiagnosticRelatedInformation](#DiagnosticRelatedInformation) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Location.is(candidate.location) && Is.string(candidate.message);
    }
    DiagnosticRelatedInformation.is = is;
})(DiagnosticRelatedInformation || (DiagnosticRelatedInformation = {}));
/**
 * The diagnostic's severity.
 */
var DiagnosticSeverity;
(function (DiagnosticSeverity) {
    /**
     * Reports an error.
     */
    DiagnosticSeverity.Error = 1;
    /**
     * Reports a warning.
     */
    DiagnosticSeverity.Warning = 2;
    /**
     * Reports an information.
     */
    DiagnosticSeverity.Information = 3;
    /**
     * Reports a hint.
     */
    DiagnosticSeverity.Hint = 4;
})(DiagnosticSeverity || (DiagnosticSeverity = {}));
/**
 * The diagnostic tags.
 *
 * @since 3.15.0
 */
var DiagnosticTag;
(function (DiagnosticTag) {
    /**
     * Unused or unnecessary code.
     *
     * Clients are allowed to render diagnostics with this tag faded out instead of having
     * an error squiggle.
     */
    DiagnosticTag.Unnecessary = 1;
    /**
     * Deprecated or obsolete code.
     *
     * Clients are allowed to rendered diagnostics with this tag strike through.
     */
    DiagnosticTag.Deprecated = 2;
})(DiagnosticTag || (DiagnosticTag = {}));
/**
 * The CodeDescription namespace provides functions to deal with descriptions for diagnostic codes.
 *
 * @since 3.16.0
 */
var CodeDescription;
(function (CodeDescription) {
    function is(value) {
        var candidate = value;
        return candidate !== undefined && candidate !== null && Is.string(candidate.href);
    }
    CodeDescription.is = is;
})(CodeDescription || (CodeDescription = {}));
/**
 * The Diagnostic namespace provides helper functions to work with
 * [Diagnostic](#Diagnostic) literals.
 */
var Diagnostic;
(function (Diagnostic) {
    /**
     * Creates a new Diagnostic literal.
     */
    function create(range, message, severity, code, source, relatedInformation) {
        var result = { range: range, message: message };
        if (Is.defined(severity)) {
            result.severity = severity;
        }
        if (Is.defined(code)) {
            result.code = code;
        }
        if (Is.defined(source)) {
            result.source = source;
        }
        if (Is.defined(relatedInformation)) {
            result.relatedInformation = relatedInformation;
        }
        return result;
    }
    Diagnostic.create = create;
    /**
     * Checks whether the given literal conforms to the [Diagnostic](#Diagnostic) interface.
     */
    function is(value) {
        var _a;
        var candidate = value;
        return Is.defined(candidate)
            && Range.is(candidate.range)
            && Is.string(candidate.message)
            && (Is.number(candidate.severity) || Is.undefined(candidate.severity))
            && (Is.integer(candidate.code) || Is.string(candidate.code) || Is.undefined(candidate.code))
            && (Is.undefined(candidate.codeDescription) || (Is.string((_a = candidate.codeDescription) === null || _a === void 0 ? void 0 : _a.href)))
            && (Is.string(candidate.source) || Is.undefined(candidate.source))
            && (Is.undefined(candidate.relatedInformation) || Is.typedArray(candidate.relatedInformation, DiagnosticRelatedInformation.is));
    }
    Diagnostic.is = is;
})(Diagnostic || (Diagnostic = {}));
/**
 * The Command namespace provides helper functions to work with
 * [Command](#Command) literals.
 */
var Command;
(function (Command) {
    /**
     * Creates a new Command literal.
     */
    function create(title, command) {
        var args = [];
        for (var _i = 2; _i < arguments.length; _i++) {
            args[_i - 2] = arguments[_i];
        }
        var result = { title: title, command: command };
        if (Is.defined(args) && args.length > 0) {
            result.arguments = args;
        }
        return result;
    }
    Command.create = create;
    /**
     * Checks whether the given literal conforms to the [Command](#Command) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.title) && Is.string(candidate.command);
    }
    Command.is = is;
})(Command || (Command = {}));
/**
 * The TextEdit namespace provides helper function to create replace,
 * insert and delete edits more easily.
 */
var TextEdit;
(function (TextEdit) {
    /**
     * Creates a replace text edit.
     * @param range The range of text to be replaced.
     * @param newText The new text.
     */
    function replace(range, newText) {
        return { range: range, newText: newText };
    }
    TextEdit.replace = replace;
    /**
     * Creates a insert text edit.
     * @param position The position to insert the text at.
     * @param newText The text to be inserted.
     */
    function insert(position, newText) {
        return { range: { start: position, end: position }, newText: newText };
    }
    TextEdit.insert = insert;
    /**
     * Creates a delete text edit.
     * @param range The range of text to be deleted.
     */
    function del(range) {
        return { range: range, newText: '' };
    }
    TextEdit.del = del;
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate)
            && Is.string(candidate.newText)
            && Range.is(candidate.range);
    }
    TextEdit.is = is;
})(TextEdit || (TextEdit = {}));
var ChangeAnnotation;
(function (ChangeAnnotation) {
    function create(label, needsConfirmation, description) {
        var result = { label: label };
        if (needsConfirmation !== undefined) {
            result.needsConfirmation = needsConfirmation;
        }
        if (description !== undefined) {
            result.description = description;
        }
        return result;
    }
    ChangeAnnotation.create = create;
    function is(value) {
        var candidate = value;
        return candidate !== undefined && Is.objectLiteral(candidate) && Is.string(candidate.label) &&
            (Is.boolean(candidate.needsConfirmation) || candidate.needsConfirmation === undefined) &&
            (Is.string(candidate.description) || candidate.description === undefined);
    }
    ChangeAnnotation.is = is;
})(ChangeAnnotation || (ChangeAnnotation = {}));
var ChangeAnnotationIdentifier;
(function (ChangeAnnotationIdentifier) {
    function is(value) {
        var candidate = value;
        return typeof candidate === 'string';
    }
    ChangeAnnotationIdentifier.is = is;
})(ChangeAnnotationIdentifier || (ChangeAnnotationIdentifier = {}));
var AnnotatedTextEdit;
(function (AnnotatedTextEdit) {
    /**
     * Creates an annotated replace text edit.
     *
     * @param range The range of text to be replaced.
     * @param newText The new text.
     * @param annotation The annotation.
     */
    function replace(range, newText, annotation) {
        return { range: range, newText: newText, annotationId: annotation };
    }
    AnnotatedTextEdit.replace = replace;
    /**
     * Creates an annotated insert text edit.
     *
     * @param position The position to insert the text at.
     * @param newText The text to be inserted.
     * @param annotation The annotation.
     */
    function insert(position, newText, annotation) {
        return { range: { start: position, end: position }, newText: newText, annotationId: annotation };
    }
    AnnotatedTextEdit.insert = insert;
    /**
     * Creates an annotated delete text edit.
     *
     * @param range The range of text to be deleted.
     * @param annotation The annotation.
     */
    function del(range, annotation) {
        return { range: range, newText: '', annotationId: annotation };
    }
    AnnotatedTextEdit.del = del;
    function is(value) {
        var candidate = value;
        return TextEdit.is(candidate) && (ChangeAnnotation.is(candidate.annotationId) || ChangeAnnotationIdentifier.is(candidate.annotationId));
    }
    AnnotatedTextEdit.is = is;
})(AnnotatedTextEdit || (AnnotatedTextEdit = {}));
/**
 * The TextDocumentEdit namespace provides helper function to create
 * an edit that manipulates a text document.
 */
var TextDocumentEdit;
(function (TextDocumentEdit) {
    /**
     * Creates a new `TextDocumentEdit`
     */
    function create(textDocument, edits) {
        return { textDocument: textDocument, edits: edits };
    }
    TextDocumentEdit.create = create;
    function is(value) {
        var candidate = value;
        return Is.defined(candidate)
            && OptionalVersionedTextDocumentIdentifier.is(candidate.textDocument)
            && Array.isArray(candidate.edits);
    }
    TextDocumentEdit.is = is;
})(TextDocumentEdit || (TextDocumentEdit = {}));
var CreateFile;
(function (CreateFile) {
    function create(uri, options, annotation) {
        var result = {
            kind: 'create',
            uri: uri
        };
        if (options !== undefined && (options.overwrite !== undefined || options.ignoreIfExists !== undefined)) {
            result.options = options;
        }
        if (annotation !== undefined) {
            result.annotationId = annotation;
        }
        return result;
    }
    CreateFile.create = create;
    function is(value) {
        var candidate = value;
        return candidate && candidate.kind === 'create' && Is.string(candidate.uri) && (candidate.options === undefined ||
            ((candidate.options.overwrite === undefined || Is.boolean(candidate.options.overwrite)) && (candidate.options.ignoreIfExists === undefined || Is.boolean(candidate.options.ignoreIfExists)))) && (candidate.annotationId === undefined || ChangeAnnotationIdentifier.is(candidate.annotationId));
    }
    CreateFile.is = is;
})(CreateFile || (CreateFile = {}));
var RenameFile;
(function (RenameFile) {
    function create(oldUri, newUri, options, annotation) {
        var result = {
            kind: 'rename',
            oldUri: oldUri,
            newUri: newUri
        };
        if (options !== undefined && (options.overwrite !== undefined || options.ignoreIfExists !== undefined)) {
            result.options = options;
        }
        if (annotation !== undefined) {
            result.annotationId = annotation;
        }
        return result;
    }
    RenameFile.create = create;
    function is(value) {
        var candidate = value;
        return candidate && candidate.kind === 'rename' && Is.string(candidate.oldUri) && Is.string(candidate.newUri) && (candidate.options === undefined ||
            ((candidate.options.overwrite === undefined || Is.boolean(candidate.options.overwrite)) && (candidate.options.ignoreIfExists === undefined || Is.boolean(candidate.options.ignoreIfExists)))) && (candidate.annotationId === undefined || ChangeAnnotationIdentifier.is(candidate.annotationId));
    }
    RenameFile.is = is;
})(RenameFile || (RenameFile = {}));
var DeleteFile;
(function (DeleteFile) {
    function create(uri, options, annotation) {
        var result = {
            kind: 'delete',
            uri: uri
        };
        if (options !== undefined && (options.recursive !== undefined || options.ignoreIfNotExists !== undefined)) {
            result.options = options;
        }
        if (annotation !== undefined) {
            result.annotationId = annotation;
        }
        return result;
    }
    DeleteFile.create = create;
    function is(value) {
        var candidate = value;
        return candidate && candidate.kind === 'delete' && Is.string(candidate.uri) && (candidate.options === undefined ||
            ((candidate.options.recursive === undefined || Is.boolean(candidate.options.recursive)) && (candidate.options.ignoreIfNotExists === undefined || Is.boolean(candidate.options.ignoreIfNotExists)))) && (candidate.annotationId === undefined || ChangeAnnotationIdentifier.is(candidate.annotationId));
    }
    DeleteFile.is = is;
})(DeleteFile || (DeleteFile = {}));
var WorkspaceEdit;
(function (WorkspaceEdit) {
    function is(value) {
        var candidate = value;
        return candidate &&
            (candidate.changes !== undefined || candidate.documentChanges !== undefined) &&
            (candidate.documentChanges === undefined || candidate.documentChanges.every(function (change) {
                if (Is.string(change.kind)) {
                    return CreateFile.is(change) || RenameFile.is(change) || DeleteFile.is(change);
                }
                else {
                    return TextDocumentEdit.is(change);
                }
            }));
    }
    WorkspaceEdit.is = is;
})(WorkspaceEdit || (WorkspaceEdit = {}));
var TextEditChangeImpl = /** @class */ (function () {
    function TextEditChangeImpl(edits, changeAnnotations) {
        this.edits = edits;
        this.changeAnnotations = changeAnnotations;
    }
    TextEditChangeImpl.prototype.insert = function (position, newText, annotation) {
        var edit;
        var id;
        if (annotation === undefined) {
            edit = TextEdit.insert(position, newText);
        }
        else if (ChangeAnnotationIdentifier.is(annotation)) {
            id = annotation;
            edit = AnnotatedTextEdit.insert(position, newText, annotation);
        }
        else {
            this.assertChangeAnnotations(this.changeAnnotations);
            id = this.changeAnnotations.manage(annotation);
            edit = AnnotatedTextEdit.insert(position, newText, id);
        }
        this.edits.push(edit);
        if (id !== undefined) {
            return id;
        }
    };
    TextEditChangeImpl.prototype.replace = function (range, newText, annotation) {
        var edit;
        var id;
        if (annotation === undefined) {
            edit = TextEdit.replace(range, newText);
        }
        else if (ChangeAnnotationIdentifier.is(annotation)) {
            id = annotation;
            edit = AnnotatedTextEdit.replace(range, newText, annotation);
        }
        else {
            this.assertChangeAnnotations(this.changeAnnotations);
            id = this.changeAnnotations.manage(annotation);
            edit = AnnotatedTextEdit.replace(range, newText, id);
        }
        this.edits.push(edit);
        if (id !== undefined) {
            return id;
        }
    };
    TextEditChangeImpl.prototype.delete = function (range, annotation) {
        var edit;
        var id;
        if (annotation === undefined) {
            edit = TextEdit.del(range);
        }
        else if (ChangeAnnotationIdentifier.is(annotation)) {
            id = annotation;
            edit = AnnotatedTextEdit.del(range, annotation);
        }
        else {
            this.assertChangeAnnotations(this.changeAnnotations);
            id = this.changeAnnotations.manage(annotation);
            edit = AnnotatedTextEdit.del(range, id);
        }
        this.edits.push(edit);
        if (id !== undefined) {
            return id;
        }
    };
    TextEditChangeImpl.prototype.add = function (edit) {
        this.edits.push(edit);
    };
    TextEditChangeImpl.prototype.all = function () {
        return this.edits;
    };
    TextEditChangeImpl.prototype.clear = function () {
        this.edits.splice(0, this.edits.length);
    };
    TextEditChangeImpl.prototype.assertChangeAnnotations = function (value) {
        if (value === undefined) {
            throw new Error("Text edit change is not configured to manage change annotations.");
        }
    };
    return TextEditChangeImpl;
}());
/**
 * A helper class
 */
var ChangeAnnotations = /** @class */ (function () {
    function ChangeAnnotations(annotations) {
        this._annotations = annotations === undefined ? Object.create(null) : annotations;
        this._counter = 0;
        this._size = 0;
    }
    ChangeAnnotations.prototype.all = function () {
        return this._annotations;
    };
    Object.defineProperty(ChangeAnnotations.prototype, "size", {
        get: function () {
            return this._size;
        },
        enumerable: false,
        configurable: true
    });
    ChangeAnnotations.prototype.manage = function (idOrAnnotation, annotation) {
        var id;
        if (ChangeAnnotationIdentifier.is(idOrAnnotation)) {
            id = idOrAnnotation;
        }
        else {
            id = this.nextId();
            annotation = idOrAnnotation;
        }
        if (this._annotations[id] !== undefined) {
            throw new Error("Id " + id + " is already in use.");
        }
        if (annotation === undefined) {
            throw new Error("No annotation provided for id " + id);
        }
        this._annotations[id] = annotation;
        this._size++;
        return id;
    };
    ChangeAnnotations.prototype.nextId = function () {
        this._counter++;
        return this._counter.toString();
    };
    return ChangeAnnotations;
}());
/**
 * A workspace change helps constructing changes to a workspace.
 */
var WorkspaceChange = /** @class */ (function () {
    function WorkspaceChange(workspaceEdit) {
        var _this = this;
        this._textEditChanges = Object.create(null);
        if (workspaceEdit !== undefined) {
            this._workspaceEdit = workspaceEdit;
            if (workspaceEdit.documentChanges) {
                this._changeAnnotations = new ChangeAnnotations(workspaceEdit.changeAnnotations);
                workspaceEdit.changeAnnotations = this._changeAnnotations.all();
                workspaceEdit.documentChanges.forEach(function (change) {
                    if (TextDocumentEdit.is(change)) {
                        var textEditChange = new TextEditChangeImpl(change.edits, _this._changeAnnotations);
                        _this._textEditChanges[change.textDocument.uri] = textEditChange;
                    }
                });
            }
            else if (workspaceEdit.changes) {
                Object.keys(workspaceEdit.changes).forEach(function (key) {
                    var textEditChange = new TextEditChangeImpl(workspaceEdit.changes[key]);
                    _this._textEditChanges[key] = textEditChange;
                });
            }
        }
        else {
            this._workspaceEdit = {};
        }
    }
    Object.defineProperty(WorkspaceChange.prototype, "edit", {
        /**
         * Returns the underlying [WorkspaceEdit](#WorkspaceEdit) literal
         * use to be returned from a workspace edit operation like rename.
         */
        get: function () {
            this.initDocumentChanges();
            if (this._changeAnnotations !== undefined) {
                if (this._changeAnnotations.size === 0) {
                    this._workspaceEdit.changeAnnotations = undefined;
                }
                else {
                    this._workspaceEdit.changeAnnotations = this._changeAnnotations.all();
                }
            }
            return this._workspaceEdit;
        },
        enumerable: false,
        configurable: true
    });
    WorkspaceChange.prototype.getTextEditChange = function (key) {
        if (OptionalVersionedTextDocumentIdentifier.is(key)) {
            this.initDocumentChanges();
            if (this._workspaceEdit.documentChanges === undefined) {
                throw new Error('Workspace edit is not configured for document changes.');
            }
            var textDocument = { uri: key.uri, version: key.version };
            var result = this._textEditChanges[textDocument.uri];
            if (!result) {
                var edits = [];
                var textDocumentEdit = {
                    textDocument: textDocument,
                    edits: edits
                };
                this._workspaceEdit.documentChanges.push(textDocumentEdit);
                result = new TextEditChangeImpl(edits, this._changeAnnotations);
                this._textEditChanges[textDocument.uri] = result;
            }
            return result;
        }
        else {
            this.initChanges();
            if (this._workspaceEdit.changes === undefined) {
                throw new Error('Workspace edit is not configured for normal text edit changes.');
            }
            var result = this._textEditChanges[key];
            if (!result) {
                var edits = [];
                this._workspaceEdit.changes[key] = edits;
                result = new TextEditChangeImpl(edits);
                this._textEditChanges[key] = result;
            }
            return result;
        }
    };
    WorkspaceChange.prototype.initDocumentChanges = function () {
        if (this._workspaceEdit.documentChanges === undefined && this._workspaceEdit.changes === undefined) {
            this._changeAnnotations = new ChangeAnnotations();
            this._workspaceEdit.documentChanges = [];
            this._workspaceEdit.changeAnnotations = this._changeAnnotations.all();
        }
    };
    WorkspaceChange.prototype.initChanges = function () {
        if (this._workspaceEdit.documentChanges === undefined && this._workspaceEdit.changes === undefined) {
            this._workspaceEdit.changes = Object.create(null);
        }
    };
    WorkspaceChange.prototype.createFile = function (uri, optionsOrAnnotation, options) {
        this.initDocumentChanges();
        if (this._workspaceEdit.documentChanges === undefined) {
            throw new Error('Workspace edit is not configured for document changes.');
        }
        var annotation;
        if (ChangeAnnotation.is(optionsOrAnnotation) || ChangeAnnotationIdentifier.is(optionsOrAnnotation)) {
            annotation = optionsOrAnnotation;
        }
        else {
            options = optionsOrAnnotation;
        }
        var operation;
        var id;
        if (annotation === undefined) {
            operation = CreateFile.create(uri, options);
        }
        else {
            id = ChangeAnnotationIdentifier.is(annotation) ? annotation : this._changeAnnotations.manage(annotation);
            operation = CreateFile.create(uri, options, id);
        }
        this._workspaceEdit.documentChanges.push(operation);
        if (id !== undefined) {
            return id;
        }
    };
    WorkspaceChange.prototype.renameFile = function (oldUri, newUri, optionsOrAnnotation, options) {
        this.initDocumentChanges();
        if (this._workspaceEdit.documentChanges === undefined) {
            throw new Error('Workspace edit is not configured for document changes.');
        }
        var annotation;
        if (ChangeAnnotation.is(optionsOrAnnotation) || ChangeAnnotationIdentifier.is(optionsOrAnnotation)) {
            annotation = optionsOrAnnotation;
        }
        else {
            options = optionsOrAnnotation;
        }
        var operation;
        var id;
        if (annotation === undefined) {
            operation = RenameFile.create(oldUri, newUri, options);
        }
        else {
            id = ChangeAnnotationIdentifier.is(annotation) ? annotation : this._changeAnnotations.manage(annotation);
            operation = RenameFile.create(oldUri, newUri, options, id);
        }
        this._workspaceEdit.documentChanges.push(operation);
        if (id !== undefined) {
            return id;
        }
    };
    WorkspaceChange.prototype.deleteFile = function (uri, optionsOrAnnotation, options) {
        this.initDocumentChanges();
        if (this._workspaceEdit.documentChanges === undefined) {
            throw new Error('Workspace edit is not configured for document changes.');
        }
        var annotation;
        if (ChangeAnnotation.is(optionsOrAnnotation) || ChangeAnnotationIdentifier.is(optionsOrAnnotation)) {
            annotation = optionsOrAnnotation;
        }
        else {
            options = optionsOrAnnotation;
        }
        var operation;
        var id;
        if (annotation === undefined) {
            operation = DeleteFile.create(uri, options);
        }
        else {
            id = ChangeAnnotationIdentifier.is(annotation) ? annotation : this._changeAnnotations.manage(annotation);
            operation = DeleteFile.create(uri, options, id);
        }
        this._workspaceEdit.documentChanges.push(operation);
        if (id !== undefined) {
            return id;
        }
    };
    return WorkspaceChange;
}());

/**
 * The TextDocumentIdentifier namespace provides helper functions to work with
 * [TextDocumentIdentifier](#TextDocumentIdentifier) literals.
 */
var TextDocumentIdentifier;
(function (TextDocumentIdentifier) {
    /**
     * Creates a new TextDocumentIdentifier literal.
     * @param uri The document's uri.
     */
    function create(uri) {
        return { uri: uri };
    }
    TextDocumentIdentifier.create = create;
    /**
     * Checks whether the given literal conforms to the [TextDocumentIdentifier](#TextDocumentIdentifier) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri);
    }
    TextDocumentIdentifier.is = is;
})(TextDocumentIdentifier || (TextDocumentIdentifier = {}));
/**
 * The VersionedTextDocumentIdentifier namespace provides helper functions to work with
 * [VersionedTextDocumentIdentifier](#VersionedTextDocumentIdentifier) literals.
 */
var VersionedTextDocumentIdentifier;
(function (VersionedTextDocumentIdentifier) {
    /**
     * Creates a new VersionedTextDocumentIdentifier literal.
     * @param uri The document's uri.
     * @param uri The document's text.
     */
    function create(uri, version) {
        return { uri: uri, version: version };
    }
    VersionedTextDocumentIdentifier.create = create;
    /**
     * Checks whether the given literal conforms to the [VersionedTextDocumentIdentifier](#VersionedTextDocumentIdentifier) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && Is.integer(candidate.version);
    }
    VersionedTextDocumentIdentifier.is = is;
})(VersionedTextDocumentIdentifier || (VersionedTextDocumentIdentifier = {}));
/**
 * The OptionalVersionedTextDocumentIdentifier namespace provides helper functions to work with
 * [OptionalVersionedTextDocumentIdentifier](#OptionalVersionedTextDocumentIdentifier) literals.
 */
var OptionalVersionedTextDocumentIdentifier;
(function (OptionalVersionedTextDocumentIdentifier) {
    /**
     * Creates a new OptionalVersionedTextDocumentIdentifier literal.
     * @param uri The document's uri.
     * @param uri The document's text.
     */
    function create(uri, version) {
        return { uri: uri, version: version };
    }
    OptionalVersionedTextDocumentIdentifier.create = create;
    /**
     * Checks whether the given literal conforms to the [OptionalVersionedTextDocumentIdentifier](#OptionalVersionedTextDocumentIdentifier) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && (candidate.version === null || Is.integer(candidate.version));
    }
    OptionalVersionedTextDocumentIdentifier.is = is;
})(OptionalVersionedTextDocumentIdentifier || (OptionalVersionedTextDocumentIdentifier = {}));
/**
 * The TextDocumentItem namespace provides helper functions to work with
 * [TextDocumentItem](#TextDocumentItem) literals.
 */
var TextDocumentItem;
(function (TextDocumentItem) {
    /**
     * Creates a new TextDocumentItem literal.
     * @param uri The document's uri.
     * @param languageId The document's language identifier.
     * @param version The document's version number.
     * @param text The document's text.
     */
    function create(uri, languageId, version, text) {
        return { uri: uri, languageId: languageId, version: version, text: text };
    }
    TextDocumentItem.create = create;
    /**
     * Checks whether the given literal conforms to the [TextDocumentItem](#TextDocumentItem) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && Is.string(candidate.languageId) && Is.integer(candidate.version) && Is.string(candidate.text);
    }
    TextDocumentItem.is = is;
})(TextDocumentItem || (TextDocumentItem = {}));
/**
 * Describes the content type that a client supports in various
 * result literals like `Hover`, `ParameterInfo` or `CompletionItem`.
 *
 * Please note that `MarkupKinds` must not start with a `$`. This kinds
 * are reserved for internal usage.
 */
var MarkupKind;
(function (MarkupKind) {
    /**
     * Plain text is supported as a content format
     */
    MarkupKind.PlainText = 'plaintext';
    /**
     * Markdown is supported as a content format
     */
    MarkupKind.Markdown = 'markdown';
})(MarkupKind || (MarkupKind = {}));
(function (MarkupKind) {
    /**
     * Checks whether the given value is a value of the [MarkupKind](#MarkupKind) type.
     */
    function is(value) {
        var candidate = value;
        return candidate === MarkupKind.PlainText || candidate === MarkupKind.Markdown;
    }
    MarkupKind.is = is;
})(MarkupKind || (MarkupKind = {}));
var MarkupContent;
(function (MarkupContent) {
    /**
     * Checks whether the given value conforms to the [MarkupContent](#MarkupContent) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(value) && MarkupKind.is(candidate.kind) && Is.string(candidate.value);
    }
    MarkupContent.is = is;
})(MarkupContent || (MarkupContent = {}));
/**
 * The kind of a completion entry.
 */
var CompletionItemKind;
(function (CompletionItemKind) {
    CompletionItemKind.Text = 1;
    CompletionItemKind.Method = 2;
    CompletionItemKind.Function = 3;
    CompletionItemKind.Constructor = 4;
    CompletionItemKind.Field = 5;
    CompletionItemKind.Variable = 6;
    CompletionItemKind.Class = 7;
    CompletionItemKind.Interface = 8;
    CompletionItemKind.Module = 9;
    CompletionItemKind.Property = 10;
    CompletionItemKind.Unit = 11;
    CompletionItemKind.Value = 12;
    CompletionItemKind.Enum = 13;
    CompletionItemKind.Keyword = 14;
    CompletionItemKind.Snippet = 15;
    CompletionItemKind.Color = 16;
    CompletionItemKind.File = 17;
    CompletionItemKind.Reference = 18;
    CompletionItemKind.Folder = 19;
    CompletionItemKind.EnumMember = 20;
    CompletionItemKind.Constant = 21;
    CompletionItemKind.Struct = 22;
    CompletionItemKind.Event = 23;
    CompletionItemKind.Operator = 24;
    CompletionItemKind.TypeParameter = 25;
})(CompletionItemKind || (CompletionItemKind = {}));
/**
 * Defines whether the insert text in a completion item should be interpreted as
 * plain text or a snippet.
 */
var InsertTextFormat;
(function (InsertTextFormat) {
    /**
     * The primary text to be inserted is treated as a plain string.
     */
    InsertTextFormat.PlainText = 1;
    /**
     * The primary text to be inserted is treated as a snippet.
     *
     * A snippet can define tab stops and placeholders with `$1`, `$2`
     * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
     * the end of the snippet. Placeholders with equal identifiers are linked,
     * that is typing in one will update others too.
     *
     * See also: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#snippet_syntax
     */
    InsertTextFormat.Snippet = 2;
})(InsertTextFormat || (InsertTextFormat = {}));
/**
 * Completion item tags are extra annotations that tweak the rendering of a completion
 * item.
 *
 * @since 3.15.0
 */
var CompletionItemTag;
(function (CompletionItemTag) {
    /**
     * Render a completion as obsolete, usually using a strike-out.
     */
    CompletionItemTag.Deprecated = 1;
})(CompletionItemTag || (CompletionItemTag = {}));
/**
 * The InsertReplaceEdit namespace provides functions to deal with insert / replace edits.
 *
 * @since 3.16.0
 */
var InsertReplaceEdit;
(function (InsertReplaceEdit) {
    /**
     * Creates a new insert / replace edit
     */
    function create(newText, insert, replace) {
        return { newText: newText, insert: insert, replace: replace };
    }
    InsertReplaceEdit.create = create;
    /**
     * Checks whether the given literal conforms to the [InsertReplaceEdit](#InsertReplaceEdit) interface.
     */
    function is(value) {
        var candidate = value;
        return candidate && Is.string(candidate.newText) && Range.is(candidate.insert) && Range.is(candidate.replace);
    }
    InsertReplaceEdit.is = is;
})(InsertReplaceEdit || (InsertReplaceEdit = {}));
/**
 * How whitespace and indentation is handled during completion
 * item insertion.
 *
 * @since 3.16.0
 */
var InsertTextMode;
(function (InsertTextMode) {
    /**
     * The insertion or replace strings is taken as it is. If the
     * value is multi line the lines below the cursor will be
     * inserted using the indentation defined in the string value.
     * The client will not apply any kind of adjustments to the
     * string.
     */
    InsertTextMode.asIs = 1;
    /**
     * The editor adjusts leading whitespace of new lines so that
     * they match the indentation up to the cursor of the line for
     * which the item is accepted.
     *
     * Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a
     * multi line completion item is indented using 2 tabs and all
     * following lines inserted will be indented using 2 tabs as well.
     */
    InsertTextMode.adjustIndentation = 2;
})(InsertTextMode || (InsertTextMode = {}));
/**
 * The CompletionItem namespace provides functions to deal with
 * completion items.
 */
var CompletionItem;
(function (CompletionItem) {
    /**
     * Create a completion item and seed it with a label.
     * @param label The completion item's label
     */
    function create(label) {
        return { label: label };
    }
    CompletionItem.create = create;
})(CompletionItem || (CompletionItem = {}));
/**
 * The CompletionList namespace provides functions to deal with
 * completion lists.
 */
var CompletionList;
(function (CompletionList) {
    /**
     * Creates a new completion list.
     *
     * @param items The completion items.
     * @param isIncomplete The list is not complete.
     */
    function create(items, isIncomplete) {
        return { items: items ? items : [], isIncomplete: !!isIncomplete };
    }
    CompletionList.create = create;
})(CompletionList || (CompletionList = {}));
var MarkedString;
(function (MarkedString) {
    /**
     * Creates a marked string from plain text.
     *
     * @param plainText The plain text.
     */
    function fromPlainText(plainText) {
        return plainText.replace(/[\\`*_{}[\]()#+\-.!]/g, '\\$&'); // escape markdown syntax tokens: http://daringfireball.net/projects/markdown/syntax#backslash
    }
    MarkedString.fromPlainText = fromPlainText;
    /**
     * Checks whether the given value conforms to the [MarkedString](#MarkedString) type.
     */
    function is(value) {
        var candidate = value;
        return Is.string(candidate) || (Is.objectLiteral(candidate) && Is.string(candidate.language) && Is.string(candidate.value));
    }
    MarkedString.is = is;
})(MarkedString || (MarkedString = {}));
var Hover;
(function (Hover) {
    /**
     * Checks whether the given value conforms to the [Hover](#Hover) interface.
     */
    function is(value) {
        var candidate = value;
        return !!candidate && Is.objectLiteral(candidate) && (MarkupContent.is(candidate.contents) ||
            MarkedString.is(candidate.contents) ||
            Is.typedArray(candidate.contents, MarkedString.is)) && (value.range === undefined || Range.is(value.range));
    }
    Hover.is = is;
})(Hover || (Hover = {}));
/**
 * The ParameterInformation namespace provides helper functions to work with
 * [ParameterInformation](#ParameterInformation) literals.
 */
var ParameterInformation;
(function (ParameterInformation) {
    /**
     * Creates a new parameter information literal.
     *
     * @param label A label string.
     * @param documentation A doc string.
     */
    function create(label, documentation) {
        return documentation ? { label: label, documentation: documentation } : { label: label };
    }
    ParameterInformation.create = create;
})(ParameterInformation || (ParameterInformation = {}));
/**
 * The SignatureInformation namespace provides helper functions to work with
 * [SignatureInformation](#SignatureInformation) literals.
 */
var SignatureInformation;
(function (SignatureInformation) {
    function create(label, documentation) {
        var parameters = [];
        for (var _i = 2; _i < arguments.length; _i++) {
            parameters[_i - 2] = arguments[_i];
        }
        var result = { label: label };
        if (Is.defined(documentation)) {
            result.documentation = documentation;
        }
        if (Is.defined(parameters)) {
            result.parameters = parameters;
        }
        else {
            result.parameters = [];
        }
        return result;
    }
    SignatureInformation.create = create;
})(SignatureInformation || (SignatureInformation = {}));
/**
 * A document highlight kind.
 */
var DocumentHighlightKind;
(function (DocumentHighlightKind) {
    /**
     * A textual occurrence.
     */
    DocumentHighlightKind.Text = 1;
    /**
     * Read-access of a symbol, like reading a variable.
     */
    DocumentHighlightKind.Read = 2;
    /**
     * Write-access of a symbol, like writing to a variable.
     */
    DocumentHighlightKind.Write = 3;
})(DocumentHighlightKind || (DocumentHighlightKind = {}));
/**
 * DocumentHighlight namespace to provide helper functions to work with
 * [DocumentHighlight](#DocumentHighlight) literals.
 */
var DocumentHighlight;
(function (DocumentHighlight) {
    /**
     * Create a DocumentHighlight object.
     * @param range The range the highlight applies to.
     */
    function create(range, kind) {
        var result = { range: range };
        if (Is.number(kind)) {
            result.kind = kind;
        }
        return result;
    }
    DocumentHighlight.create = create;
})(DocumentHighlight || (DocumentHighlight = {}));
/**
 * A symbol kind.
 */
var SymbolKind;
(function (SymbolKind) {
    SymbolKind.File = 1;
    SymbolKind.Module = 2;
    SymbolKind.Namespace = 3;
    SymbolKind.Package = 4;
    SymbolKind.Class = 5;
    SymbolKind.Method = 6;
    SymbolKind.Property = 7;
    SymbolKind.Field = 8;
    SymbolKind.Constructor = 9;
    SymbolKind.Enum = 10;
    SymbolKind.Interface = 11;
    SymbolKind.Function = 12;
    SymbolKind.Variable = 13;
    SymbolKind.Constant = 14;
    SymbolKind.String = 15;
    SymbolKind.Number = 16;
    SymbolKind.Boolean = 17;
    SymbolKind.Array = 18;
    SymbolKind.Object = 19;
    SymbolKind.Key = 20;
    SymbolKind.Null = 21;
    SymbolKind.EnumMember = 22;
    SymbolKind.Struct = 23;
    SymbolKind.Event = 24;
    SymbolKind.Operator = 25;
    SymbolKind.TypeParameter = 26;
})(SymbolKind || (SymbolKind = {}));
/**
 * Symbol tags are extra annotations that tweak the rendering of a symbol.
 * @since 3.16
 */
var SymbolTag;
(function (SymbolTag) {
    /**
     * Render a symbol as obsolete, usually using a strike-out.
     */
    SymbolTag.Deprecated = 1;
})(SymbolTag || (SymbolTag = {}));
var SymbolInformation;
(function (SymbolInformation) {
    /**
     * Creates a new symbol information literal.
     *
     * @param name The name of the symbol.
     * @param kind The kind of the symbol.
     * @param range The range of the location of the symbol.
     * @param uri The resource of the location of symbol, defaults to the current document.
     * @param containerName The name of the symbol containing the symbol.
     */
    function create(name, kind, range, uri, containerName) {
        var result = {
            name: name,
            kind: kind,
            location: { uri: uri, range: range }
        };
        if (containerName) {
            result.containerName = containerName;
        }
        return result;
    }
    SymbolInformation.create = create;
})(SymbolInformation || (SymbolInformation = {}));
var DocumentSymbol;
(function (DocumentSymbol) {
    /**
     * Creates a new symbol information literal.
     *
     * @param name The name of the symbol.
     * @param detail The detail of the symbol.
     * @param kind The kind of the symbol.
     * @param range The range of the symbol.
     * @param selectionRange The selectionRange of the symbol.
     * @param children Children of the symbol.
     */
    function create(name, detail, kind, range, selectionRange, children) {
        var result = {
            name: name,
            detail: detail,
            kind: kind,
            range: range,
            selectionRange: selectionRange
        };
        if (children !== undefined) {
            result.children = children;
        }
        return result;
    }
    DocumentSymbol.create = create;
    /**
     * Checks whether the given literal conforms to the [DocumentSymbol](#DocumentSymbol) interface.
     */
    function is(value) {
        var candidate = value;
        return candidate &&
            Is.string(candidate.name) && Is.number(candidate.kind) &&
            Range.is(candidate.range) && Range.is(candidate.selectionRange) &&
            (candidate.detail === undefined || Is.string(candidate.detail)) &&
            (candidate.deprecated === undefined || Is.boolean(candidate.deprecated)) &&
            (candidate.children === undefined || Array.isArray(candidate.children)) &&
            (candidate.tags === undefined || Array.isArray(candidate.tags));
    }
    DocumentSymbol.is = is;
})(DocumentSymbol || (DocumentSymbol = {}));
/**
 * A set of predefined code action kinds
 */
var CodeActionKind;
(function (CodeActionKind) {
    /**
     * Empty kind.
     */
    CodeActionKind.Empty = '';
    /**
     * Base kind for quickfix actions: 'quickfix'
     */
    CodeActionKind.QuickFix = 'quickfix';
    /**
     * Base kind for refactoring actions: 'refactor'
     */
    CodeActionKind.Refactor = 'refactor';
    /**
     * Base kind for refactoring extraction actions: 'refactor.extract'
     *
     * Example extract actions:
     *
     * - Extract method
     * - Extract function
     * - Extract variable
     * - Extract interface from class
     * - ...
     */
    CodeActionKind.RefactorExtract = 'refactor.extract';
    /**
     * Base kind for refactoring inline actions: 'refactor.inline'
     *
     * Example inline actions:
     *
     * - Inline function
     * - Inline variable
     * - Inline constant
     * - ...
     */
    CodeActionKind.RefactorInline = 'refactor.inline';
    /**
     * Base kind for refactoring rewrite actions: 'refactor.rewrite'
     *
     * Example rewrite actions:
     *
     * - Convert JavaScript function to class
     * - Add or remove parameter
     * - Encapsulate field
     * - Make method static
     * - Move method to base class
     * - ...
     */
    CodeActionKind.RefactorRewrite = 'refactor.rewrite';
    /**
     * Base kind for source actions: `source`
     *
     * Source code actions apply to the entire file.
     */
    CodeActionKind.Source = 'source';
    /**
     * Base kind for an organize imports source action: `source.organizeImports`
     */
    CodeActionKind.SourceOrganizeImports = 'source.organizeImports';
    /**
     * Base kind for auto-fix source actions: `source.fixAll`.
     *
     * Fix all actions automatically fix errors that have a clear fix that do not require user input.
     * They should not suppress errors or perform unsafe fixes such as generating new types or classes.
     *
     * @since 3.15.0
     */
    CodeActionKind.SourceFixAll = 'source.fixAll';
})(CodeActionKind || (CodeActionKind = {}));
/**
 * The CodeActionContext namespace provides helper functions to work with
 * [CodeActionContext](#CodeActionContext) literals.
 */
var CodeActionContext;
(function (CodeActionContext) {
    /**
     * Creates a new CodeActionContext literal.
     */
    function create(diagnostics, only) {
        var result = { diagnostics: diagnostics };
        if (only !== undefined && only !== null) {
            result.only = only;
        }
        return result;
    }
    CodeActionContext.create = create;
    /**
     * Checks whether the given literal conforms to the [CodeActionContext](#CodeActionContext) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.typedArray(candidate.diagnostics, Diagnostic.is) && (candidate.only === undefined || Is.typedArray(candidate.only, Is.string));
    }
    CodeActionContext.is = is;
})(CodeActionContext || (CodeActionContext = {}));
var CodeAction;
(function (CodeAction) {
    function create(title, kindOrCommandOrEdit, kind) {
        var result = { title: title };
        var checkKind = true;
        if (typeof kindOrCommandOrEdit === 'string') {
            checkKind = false;
            result.kind = kindOrCommandOrEdit;
        }
        else if (Command.is(kindOrCommandOrEdit)) {
            result.command = kindOrCommandOrEdit;
        }
        else {
            result.edit = kindOrCommandOrEdit;
        }
        if (checkKind && kind !== undefined) {
            result.kind = kind;
        }
        return result;
    }
    CodeAction.create = create;
    function is(value) {
        var candidate = value;
        return candidate && Is.string(candidate.title) &&
            (candidate.diagnostics === undefined || Is.typedArray(candidate.diagnostics, Diagnostic.is)) &&
            (candidate.kind === undefined || Is.string(candidate.kind)) &&
            (candidate.edit !== undefined || candidate.command !== undefined) &&
            (candidate.command === undefined || Command.is(candidate.command)) &&
            (candidate.isPreferred === undefined || Is.boolean(candidate.isPreferred)) &&
            (candidate.edit === undefined || WorkspaceEdit.is(candidate.edit));
    }
    CodeAction.is = is;
})(CodeAction || (CodeAction = {}));
/**
 * The CodeLens namespace provides helper functions to work with
 * [CodeLens](#CodeLens) literals.
 */
var CodeLens;
(function (CodeLens) {
    /**
     * Creates a new CodeLens literal.
     */
    function create(range, data) {
        var result = { range: range };
        if (Is.defined(data)) {
            result.data = data;
        }
        return result;
    }
    CodeLens.create = create;
    /**
     * Checks whether the given literal conforms to the [CodeLens](#CodeLens) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && (Is.undefined(candidate.command) || Command.is(candidate.command));
    }
    CodeLens.is = is;
})(CodeLens || (CodeLens = {}));
/**
 * The FormattingOptions namespace provides helper functions to work with
 * [FormattingOptions](#FormattingOptions) literals.
 */
var FormattingOptions;
(function (FormattingOptions) {
    /**
     * Creates a new FormattingOptions literal.
     */
    function create(tabSize, insertSpaces) {
        return { tabSize: tabSize, insertSpaces: insertSpaces };
    }
    FormattingOptions.create = create;
    /**
     * Checks whether the given literal conforms to the [FormattingOptions](#FormattingOptions) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.uinteger(candidate.tabSize) && Is.boolean(candidate.insertSpaces);
    }
    FormattingOptions.is = is;
})(FormattingOptions || (FormattingOptions = {}));
/**
 * The DocumentLink namespace provides helper functions to work with
 * [DocumentLink](#DocumentLink) literals.
 */
var DocumentLink;
(function (DocumentLink) {
    /**
     * Creates a new DocumentLink literal.
     */
    function create(range, target, data) {
        return { range: range, target: target, data: data };
    }
    DocumentLink.create = create;
    /**
     * Checks whether the given literal conforms to the [DocumentLink](#DocumentLink) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && (Is.undefined(candidate.target) || Is.string(candidate.target));
    }
    DocumentLink.is = is;
})(DocumentLink || (DocumentLink = {}));
/**
 * The SelectionRange namespace provides helper function to work with
 * SelectionRange literals.
 */
var SelectionRange;
(function (SelectionRange) {
    /**
     * Creates a new SelectionRange
     * @param range the range.
     * @param parent an optional parent.
     */
    function create(range, parent) {
        return { range: range, parent: parent };
    }
    SelectionRange.create = create;
    function is(value) {
        var candidate = value;
        return candidate !== undefined && Range.is(candidate.range) && (candidate.parent === undefined || SelectionRange.is(candidate.parent));
    }
    SelectionRange.is = is;
})(SelectionRange || (SelectionRange = {}));
var EOL = ['\n', '\r\n', '\r'];
/**
 * @deprecated Use the text document from the new vscode-languageserver-textdocument package.
 */
var TextDocument;
(function (TextDocument) {
    /**
     * Creates a new ITextDocument literal from the given uri and content.
     * @param uri The document's uri.
     * @param languageId  The document's language Id.
     * @param content The document's content.
     */
    function create(uri, languageId, version, content) {
        return new FullTextDocument(uri, languageId, version, content);
    }
    TextDocument.create = create;
    /**
     * Checks whether the given literal conforms to the [ITextDocument](#ITextDocument) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && (Is.undefined(candidate.languageId) || Is.string(candidate.languageId)) && Is.uinteger(candidate.lineCount)
            && Is.func(candidate.getText) && Is.func(candidate.positionAt) && Is.func(candidate.offsetAt) ? true : false;
    }
    TextDocument.is = is;
    function applyEdits(document, edits) {
        var text = document.getText();
        var sortedEdits = mergeSort(edits, function (a, b) {
            var diff = a.range.start.line - b.range.start.line;
            if (diff === 0) {
                return a.range.start.character - b.range.start.character;
            }
            return diff;
        });
        var lastModifiedOffset = text.length;
        for (var i = sortedEdits.length - 1; i >= 0; i--) {
            var e = sortedEdits[i];
            var startOffset = document.offsetAt(e.range.start);
            var endOffset = document.offsetAt(e.range.end);
            if (endOffset <= lastModifiedOffset) {
                text = text.substring(0, startOffset) + e.newText + text.substring(endOffset, text.length);
            }
            else {
                throw new Error('Overlapping edit');
            }
            lastModifiedOffset = startOffset;
        }
        return text;
    }
    TextDocument.applyEdits = applyEdits;
    function mergeSort(data, compare) {
        if (data.length <= 1) {
            // sorted
            return data;
        }
        var p = (data.length / 2) | 0;
        var left = data.slice(0, p);
        var right = data.slice(p);
        mergeSort(left, compare);
        mergeSort(right, compare);
        var leftIdx = 0;
        var rightIdx = 0;
        var i = 0;
        while (leftIdx < left.length && rightIdx < right.length) {
            var ret = compare(left[leftIdx], right[rightIdx]);
            if (ret <= 0) {
                // smaller_equal -> take left to preserve order
                data[i++] = left[leftIdx++];
            }
            else {
                // greater -> take right
                data[i++] = right[rightIdx++];
            }
        }
        while (leftIdx < left.length) {
            data[i++] = left[leftIdx++];
        }
        while (rightIdx < right.length) {
            data[i++] = right[rightIdx++];
        }
        return data;
    }
})(TextDocument || (TextDocument = {}));
/**
 * @deprecated Use the text document from the new vscode-languageserver-textdocument package.
 */
var FullTextDocument = /** @class */ (function () {
    function FullTextDocument(uri, languageId, version, content) {
        this._uri = uri;
        this._languageId = languageId;
        this._version = version;
        this._content = content;
        this._lineOffsets = undefined;
    }
    Object.defineProperty(FullTextDocument.prototype, "uri", {
        get: function () {
            return this._uri;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(FullTextDocument.prototype, "languageId", {
        get: function () {
            return this._languageId;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(FullTextDocument.prototype, "version", {
        get: function () {
            return this._version;
        },
        enumerable: false,
        configurable: true
    });
    FullTextDocument.prototype.getText = function (range) {
        if (range) {
            var start = this.offsetAt(range.start);
            var end = this.offsetAt(range.end);
            return this._content.substring(start, end);
        }
        return this._content;
    };
    FullTextDocument.prototype.update = function (event, version) {
        this._content = event.text;
        this._version = version;
        this._lineOffsets = undefined;
    };
    FullTextDocument.prototype.getLineOffsets = function () {
        if (this._lineOffsets === undefined) {
            var lineOffsets = [];
            var text = this._content;
            var isLineStart = true;
            for (var i = 0; i < text.length; i++) {
                if (isLineStart) {
                    lineOffsets.push(i);
                    isLineStart = false;
                }
                var ch = text.charAt(i);
                isLineStart = (ch === '\r' || ch === '\n');
                if (ch === '\r' && i + 1 < text.length && text.charAt(i + 1) === '\n') {
                    i++;
                }
            }
            if (isLineStart && text.length > 0) {
                lineOffsets.push(text.length);
            }
            this._lineOffsets = lineOffsets;
        }
        return this._lineOffsets;
    };
    FullTextDocument.prototype.positionAt = function (offset) {
        offset = Math.max(Math.min(offset, this._content.length), 0);
        var lineOffsets = this.getLineOffsets();
        var low = 0, high = lineOffsets.length;
        if (high === 0) {
            return Position.create(0, offset);
        }
        while (low < high) {
            var mid = Math.floor((low + high) / 2);
            if (lineOffsets[mid] > offset) {
                high = mid;
            }
            else {
                low = mid + 1;
            }
        }
        // low is the least x for which the line offset is larger than the current offset
        // or array.length if no line offset is larger than the current offset
        var line = low - 1;
        return Position.create(line, offset - lineOffsets[line]);
    };
    FullTextDocument.prototype.offsetAt = function (position) {
        var lineOffsets = this.getLineOffsets();
        if (position.line >= lineOffsets.length) {
            return this._content.length;
        }
        else if (position.line < 0) {
            return 0;
        }
        var lineOffset = lineOffsets[position.line];
        var nextLineOffset = (position.line + 1 < lineOffsets.length) ? lineOffsets[position.line + 1] : this._content.length;
        return Math.max(Math.min(lineOffset + position.character, nextLineOffset), lineOffset);
    };
    Object.defineProperty(FullTextDocument.prototype, "lineCount", {
        get: function () {
            return this.getLineOffsets().length;
        },
        enumerable: false,
        configurable: true
    });
    return FullTextDocument;
}());
var Is;
(function (Is) {
    var toString = Object.prototype.toString;
    function defined(value) {
        return typeof value !== 'undefined';
    }
    Is.defined = defined;
    function undefined(value) {
        return typeof value === 'undefined';
    }
    Is.undefined = undefined;
    function boolean(value) {
        return value === true || value === false;
    }
    Is.boolean = boolean;
    function string(value) {
        return toString.call(value) === '[object String]';
    }
    Is.string = string;
    function number(value) {
        return toString.call(value) === '[object Number]';
    }
    Is.number = number;
    function numberRange(value, min, max) {
        return toString.call(value) === '[object Number]' && min <= value && value <= max;
    }
    Is.numberRange = numberRange;
    function integer(value) {
        return toString.call(value) === '[object Number]' && -2147483648 <= value && value <= 2147483647;
    }
    Is.integer = integer;
    function uinteger(value) {
        return toString.call(value) === '[object Number]' && 0 <= value && value <= 2147483647;
    }
    Is.uinteger = uinteger;
    function func(value) {
        return toString.call(value) === '[object Function]';
    }
    Is.func = func;
    function objectLiteral(value) {
        // Strictly speaking class instances pass this check as well. Since the LSP
        // doesn't use classes we ignore this for now. If we do we need to add something
        // like this: `Object.getPrototypeOf(Object.getPrototypeOf(x)) === null`
        return value !== null && typeof value === 'object';
    }
    Is.objectLiteral = objectLiteral;
    function typedArray(value, check) {
        return Array.isArray(value) && value.every(check);
    }
    Is.typedArray = typedArray;
})(Is || (Is = {}));


/***/ }),

/***/ "path":
/*!***********************!*\
  !*** external "path" ***!
  \***********************/
/***/ ((module) => {

"use strict";
module.exports = require("path");;

/***/ }),

/***/ "vscode":
/*!*************************!*\
  !*** external "vscode" ***!
  \*************************/
/***/ ((module) => {

"use strict";
module.exports = require("vscode");;

/***/ })

/******/ 	});
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		var cachedModule = __webpack_module_cache__[moduleId];
/******/ 		if (cachedModule !== undefined) {
/******/ 			return cachedModule.exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/************************************************************************/
/******/ 	/* webpack/runtime/define property getters */
/******/ 	(() => {
/******/ 		// define getter functions for harmony exports
/******/ 		__webpack_require__.d = (exports, definition) => {
/******/ 			for(var key in definition) {
/******/ 				if(__webpack_require__.o(definition, key) && !__webpack_require__.o(exports, key)) {
/******/ 					Object.defineProperty(exports, key, { enumerable: true, get: definition[key] });
/******/ 				}
/******/ 			}
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/hasOwnProperty shorthand */
/******/ 	(() => {
/******/ 		__webpack_require__.o = (obj, prop) => (Object.prototype.hasOwnProperty.call(obj, prop))
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/make namespace object */
/******/ 	(() => {
/******/ 		// define __esModule on exports
/******/ 		__webpack_require__.r = (exports) => {
/******/ 			if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 				Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 			}
/******/ 			Object.defineProperty(exports, '__esModule', { value: true });
/******/ 		};
/******/ 	})();
/******/ 	
/************************************************************************/
var __webpack_exports__ = {};
// This entry need to be wrapped in an IIFE because it need to be in strict mode.
(() => {
"use strict";
var exports = __webpack_exports__;
/*!*********************************************!*\
  !*** ./out/lang-server/client/extension.js ***!
  \*********************************************/

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.deactivate = exports.activate = void 0;
const path = __webpack_require__(/*! path */ "path");
const vscode_1 = __webpack_require__(/*! vscode */ "vscode");
const node_1 = __webpack_require__(/*! vscode-languageclient/node */ "./src/lang-server/client/node_modules/vscode-languageclient/node.js");
const compiler_1 = __webpack_require__(/*! compiler */ "./out/compiler/index.js");
let client;
function activate(context) {
    const scanner = compiler_1.Scanner("xyz");
    // The server is implemented in node
    let serverModule = context.asAbsolutePath(path.join('server', 'out', 'server.js'));
    // The debug options for the server
    // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
    let debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    let serverOptions = {
        run: { module: serverModule, transport: node_1.TransportKind.ipc },
        debug: {
            module: serverModule,
            transport: node_1.TransportKind.ipc,
            options: debugOptions
        }
    };
    // Options to control the language client
    let clientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: 'file', language: 'plaintext' }],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: vscode_1.workspace.createFileSystemWatcher('**/.clientrc')
        }
    };
    // Create the language client and start the client.
    client = new node_1.LanguageClient('languageServerExample', 'Language Server Example', serverOptions, clientOptions);
    // Start the client. This will also launch the server
    client.start();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map
})();

module.exports = __webpack_exports__;
/******/ })()
;
//# sourceMappingURL=cfls.js.map