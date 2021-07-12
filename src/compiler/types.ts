import { ArrowFunctionDefinition, BooleanLiteral, CfTag, DottedPath, FunctionDefinition, mergeRanges, NilTerminal, NodeBase, NodeType, NumericLiteral, Script, SimpleStringLiteral, SymTabEntry, Tag, TagAttribute, Terminal } from "./node";
import { SourceRange } from "./scanner";
import { getAttributeValue, getTriviallyComputableBoolean, getTriviallyComputableString } from "./utils";

let debugTypeModule = true;

export const enum TypeKind {
    any = 1, // start at non-zero
    void,
    string,
    number,
    boolean,
    nil,
    never,
    array,
    tuple,
    struct,
    union,
    intersection,
    functionSignature,
    functionSignatureParam,
    typeConstructor,
    typeConstructorInvocation,
    cachedTypeConstructorInvocation,
    typeFunctionParam,
    typeId,
}

const TypeKindUiString : Record<TypeKind, string> = {
    [TypeKind.any]:                             "any",
    [TypeKind.void]:                            "void",
    [TypeKind.string]:                          "string",
    [TypeKind.number]:                          "number",
    [TypeKind.boolean]:                         "boolean",
    [TypeKind.nil]:                             "nil",
    [TypeKind.array]:                           "array",
    [TypeKind.tuple]:                           "tuple",
    [TypeKind.struct]:                          "struct",
    [TypeKind.union]:                           "union",
    [TypeKind.intersection]:                    "intersection",
    [TypeKind.functionSignature]:               "function-signature",  // (name: type, ...) => type
    [TypeKind.functionSignatureParam]:          "function-signature-param",
    [TypeKind.typeConstructorInvocation]:       "type-constructor-invocation",           // typename | typename<type-list> where `typename` is shorthand for `typename<>` with 0 args
    [TypeKind.cachedTypeConstructorInvocation]: "cached-type-constructor-invocation",    // same as a type call but we can see that is cached; sort of a "type call closure" with inital args captured
    [TypeKind.typeConstructor]:                 "type-constructor",    // type<type-list, ...> => type
    [TypeKind.typeFunctionParam]:               "type-function-param", // type param in a type function
    [TypeKind.typeId]:                          "type-id",             // name of non-builtin type, e.g, "T"
    [TypeKind.never]:                           "never",
}

export const enum TypeFlags {
    none = 0,
    any                = 1 << 1, 
    final              = 1 << 2, 
    containsUndefined  = 1 << 3, 
    required           = 1 << 4,
    synthetic          = 1 << 5,
    spread             = 1 << 6,
    END                = 1 << 8,
}

const TypeFlagsUiString : Record<TypeFlags, string> = {
    [TypeFlags.none]: "none",
    [TypeFlags.any]: "any",
    [TypeFlags.final]: "final",
    [TypeFlags.containsUndefined]: "undefined",
    [TypeFlags.required]: "required",
    [TypeFlags.synthetic]: "synthetic",
    [TypeFlags.END]: "<<end>>"
};

export interface TypeBase extends NodeBase {
    kind: NodeType.type,
    typeKind: TypeKind,
    synthetic: boolean,
    typeFlags: TypeFlags,

    uiName?: string,
    __debug_kind?: string,
}

export function TypeBase<T extends Type>(typeKind: T["typeKind"], range?: SourceRange) : T {
    const result = NodeBase<Type>(NodeType.type, range);
    result.typeKind = typeKind;
    result.synthetic = false;
    result.typeFlags = TypeFlags.none;

    if (debugTypeModule) {
        result.__debug_kind = TypeKindUiString[typeKind];
        Object.defineProperty(
            result,
            "__debug_typeFlags", {
            get(this: TypeBase) {
                const result : string[] = [];
                for (let i = 1; (1 << i) < TypeFlags.END; i++) {
                    if (this.typeFlags & (1 << i)) {
                        result.push(TypeFlagsUiString[(1 << i) as TypeFlags]);
                    }
                }
                return result.join(", ");
            }
        });
    }

    return result as T;
}

export interface _Type {
    flags: TypeFlags,
    type: Type
}

export type Type =
    | cfAny | cfVoid | cfString | cfNumber | cfBoolean | cfNil | cfArray
    | cfUnion | cfIntersection
    | cfTypeId | cfNever
    | cfTuple | cfStruct | cfFunctionSignature | cfFunctionSignatureParam
    | cfTypeConstructorInvocation | cfCachedTypeConstructorInvocation | cfTypeConstructor | cfTypeConstructorParam;

export interface cfAny extends TypeBase {
    typeKind: TypeKind.any;
    terminal: Terminal
}

export function cfAny(terminal: Terminal) : cfAny {
    const v = TypeBase<cfAny>(TypeKind.any, terminal.range);
    v.terminal = terminal;
    return v;
}

export interface cfVoid extends TypeBase {
    typeKind: TypeKind.void
    terminal: Terminal,
}

export function cfVoid(terminal: Terminal) : cfVoid {
    const v = TypeBase<cfVoid>(TypeKind.void, terminal.range);
    v.terminal = terminal;
    return v;
}

export interface cfString extends TypeBase {
    typeKind: TypeKind.string,
    terminal: Terminal | SimpleStringLiteral,
    literal: string | null
}

export function cfString(terminal: Terminal | SimpleStringLiteral) : cfString {
    const v = TypeBase<cfString>(TypeKind.string, terminal.range);
    v.terminal = terminal;
    v.literal = terminal.kind === NodeType.simpleStringLiteral ? terminal.textSpan.text : null;
    return v;
}

export interface cfNumber extends TypeBase {
    typeKind: TypeKind.number,
    terminal: Terminal | NumericLiteral,
    literal: number | null,
}

export function cfNumber(terminal: Terminal | NumericLiteral) : cfNumber {
    const v = TypeBase<cfNumber>(TypeKind.number, terminal.range);
    v.terminal = terminal;
    v.literal = terminal.kind === NodeType.numericLiteral ? parseFloat(terminal.literal.token.text) : null;
    return v;
}

export interface cfBoolean extends TypeBase {
    typeKind: TypeKind.boolean,
    terminal: Terminal | BooleanLiteral,
    literal: boolean | null,
}

export function cfBoolean(terminal: Terminal | BooleanLiteral) : cfBoolean {
    const v = TypeBase<cfBoolean>(TypeKind.boolean, terminal.range);
    v.terminal = terminal;
    v.literal = terminal.kind === NodeType.booleanLiteral ? terminal.booleanValue : null;
    return v;
}

export interface cfNil extends TypeBase {
    typeKind: TypeKind.nil
}

export function cfNil(range?: SourceRange) : cfNil {
    const v = TypeBase<cfNil>(TypeKind.nil, range);
    return v;
}

export interface cfArray extends TypeBase {
    typeKind: TypeKind.array
    T: Type
}

export function cfArray(T: Type) : cfArray {
    const v = TypeBase<cfArray>(TypeKind.array);
    v.T = T;
    return v;
}

export interface cfTuple extends TypeBase {
    typeKind: TypeKind.tuple,
    T: Type[]
}

export function cfTuple(T: Type[]) : cfTuple {
    const v = TypeBase<cfTuple>(TypeKind.tuple);
    v.T = T;
    return v;
}

export interface cfStruct extends TypeBase {
    typeKind: TypeKind.struct,
    leftBrace: Terminal,
    members: cfStructMember[],
    rightBrace: Terminal,
    membersMap: Map<string, SymTabEntry>,
}

export function cfStruct(leftBrace: Terminal, members: cfStructMember[], membersMap: Map<string, SymTabEntry>, rightBrace: Terminal) : cfStruct {
    const v = TypeBase<cfStruct>(TypeKind.struct, mergeRanges(leftBrace, rightBrace));
    v.leftBrace = leftBrace;
    v.members = members;
    v.rightBrace = rightBrace;

    v.membersMap = membersMap;
    return v;
}

export interface cfStructMember {
    propertyName: Terminal,
    colon: Terminal,
    type: Type,
    comma: Terminal | null,
    attributes: TypeAttribute[],
}

export function cfStructMember(propertyName: Terminal, colon: Terminal, type: Type, comma: Terminal | null, attributes: TypeAttribute[]) : cfStructMember {
    return {propertyName, colon, type, comma, attributes};
}

export interface cfFunctionSignature extends TypeBase {
    typeKind: TypeKind.functionSignature,
    uiName: string,
    params: cfFunctionSignatureParam[],
    returns: Type
}

export function cfFunctionSignature(name: string, params: cfFunctionSignatureParam[], returns: Type) : cfFunctionSignature {
    const v = TypeBase<cfFunctionSignature>(TypeKind.functionSignature);
    v.uiName = name;
    v.params = params;
    v.returns = returns;
    return v;
}

export interface cfFunctionSignatureParam extends TypeBase {
    typeKind: TypeKind.functionSignatureParam,
    typeFlags: TypeFlags,
    type: Type,
    uiName: string,
    canonicalName: string,
    defaultValue: Type | null,
}

export function cfFunctionSignatureParam(typeFlags: TypeFlags, type: Type, name: string, defaultValue: Type | null) : cfFunctionSignatureParam {
    const v = TypeBase<cfFunctionSignatureParam>(TypeKind.functionSignatureParam);
    v.typeFlags = typeFlags;
    v.type = type;
    v.uiName = name;
    v.canonicalName = name.toLowerCase();
    v.defaultValue = defaultValue;
    return v;
}

export interface cfTypeConstructorInvocation extends TypeBase {
    typeKind: TypeKind.typeConstructorInvocation,
    left: Type,
    args: Type[]
}

export function cfTypeConstructorInvocation(left: Type, args: Type[]) : cfTypeConstructorInvocation {
    const v = TypeBase<cfTypeConstructorInvocation>(TypeKind.typeConstructorInvocation);
    v.left = left;
    v.args = args;
    return v;
}

export interface cfCachedTypeConstructorInvocation extends TypeBase {
    typeKind: TypeKind.cachedTypeConstructorInvocation,
    left: cfTypeConstructor,
    args: Type[],
}

export function cfCachedTypeConstructorInvocation(left: cfTypeConstructor, args: Type[]) : cfCachedTypeConstructorInvocation {
    const v = TypeBase<cfCachedTypeConstructorInvocation>(TypeKind.cachedTypeConstructorInvocation);
    v.left = left;
    v.args = args;
    return v;
}

export interface cfTypeConstructor extends TypeBase {
    typeKind: TypeKind.typeConstructor,
    uiName: string,
    params: cfTypeConstructorParam[],
    capturedParams: Map<string, Type>, // "in context Γ extended with (T0,...,Tn)"; so @type Foo = <T> => <U> => (can see T, U here)
    body: Type,
}

export function cfTypeConstructor(params: cfTypeConstructorParam[], body: Type) : cfTypeConstructor {
    const v = TypeBase<cfTypeConstructor>(TypeKind.typeConstructor);
    v.params = params;
    v.capturedParams = new Map();
    v.body = body;
    return v;
}

export interface cfTypeConstructorParam extends TypeBase {
    typeKind: TypeKind.typeFunctionParam,
    uiName: string,
    extendsToken: Terminal | null,
    extendsType: Type | null,
    comma: Terminal | null
}

export function cfTypeConstructorParam(name: string, extendsToken: Terminal | null, extendsType: Type | null, comma: Terminal | null) {
    const v = TypeBase<cfTypeConstructorParam>(TypeKind.typeFunctionParam);
    v.uiName = name;
    v.extendsToken = extendsToken;
    v.extendsType = extendsType;
    v.comma = comma;
    return v;
}

export interface cfTypeId extends TypeBase {
    typeKind: TypeKind.typeId,
    terminal: Terminal,
    uiName: string,
}

export function cfTypeId(terminal: Terminal) : cfTypeId {
    const v = TypeBase<cfTypeId>(TypeKind.typeId);
    v.range = terminal.range;
    v.terminal = terminal;
    v.uiName = terminal.token.text;
    return v;
}

export interface cfIntersection extends TypeBase {
    typeKind: TypeKind.intersection,
    left: Type,
    right: Type,
    typeList: Type[], // change parser to support this, we need a top level (&|) parser, and then a lower-level (<>=>type/{}/[]/id) parser
}

export function cfIntersection(left: Type, right: Type) : cfIntersection {
    const v = TypeBase<cfIntersection>(TypeKind.intersection);
    v.left = left;
    v.right = right;
    v.typeList = [left, right];
    return v;
}

export interface cfUnion extends TypeBase {
    typeKind: TypeKind.union,
    members: Type[],
}

export function cfUnion(...members: Type[]) {
    const v = TypeBase<cfUnion>(TypeKind.union);
    v.members = members;
    return v;
}

export interface cfNever extends TypeBase {
    typeKind: TypeKind.never,
}

export function cfNever() : cfNever {
    const v = TypeBase<cfNever>(TypeKind.never);
    return v;
}

export interface TypeAttribute extends NodeBase {
    type: NodeType.typeAttribute,
    hash: Terminal,
    exclamation: Terminal,
    leftBracket: Terminal,
    name: Terminal,
    rightBracket: Terminal,
}

export function TypeAttribute(
    hash: Terminal,
    exclamation: Terminal,
    leftBracket: Terminal,
    name: Terminal,
    rightBracket: Terminal,
) : TypeAttribute {
    const v = NodeBase<TypeAttribute>(NodeType.typeAttribute, mergeRanges(hash, rightBracket));
    v.hash = hash;
    v.exclamation = exclamation;
    v.leftBracket = leftBracket;
    v.name = name;
    v.rightBracket = rightBracket;
    return v;
}

export const SyntheticType = (function() {
    const nilTerminal = NilTerminal(-1);

    const global_any = cfAny(nilTerminal);
    const _any = () : _Type => {
        return {
            flags: TypeFlags.synthetic,
            type: global_any,
        }
    }

    const any = () => {
        const any = cfAny(nilTerminal);
        any.typeFlags |= TypeFlags.synthetic;
        return any;
    }

    const void_ = () => {
        const void_ = cfVoid(nilTerminal);
        void_.typeFlags |= TypeFlags.synthetic;
        return void_;
    }

    const string = () => {
        const string = cfString(nilTerminal);
        string.typeFlags |= TypeFlags.synthetic;
        return string;
    }

    const number = () => {
        const number = cfNumber(nilTerminal);
        number.typeFlags |= TypeFlags.synthetic;
        return number;
    }

    const boolean = () => {
        const boolean = cfBoolean(nilTerminal);
        boolean.typeFlags |= TypeFlags.synthetic;
        return boolean;
    }

    const nil = () => {
        const nil = cfNil();
        nil.typeFlags |= TypeFlags.synthetic;
        return nil;
    }

    const struct = (membersMap: Map<string, SymTabEntry> = new Map()) => {
        const v = cfStruct(nilTerminal, [], membersMap, nilTerminal);
        v.typeFlags |= TypeFlags.synthetic;
        return v;
    }

    const never = () => {
        const never = cfNever();
        never.typeFlags |= TypeFlags.synthetic;
        return never;
    }

    /**
     * goal would be something like
     * type Query = <T extends struct> => {
     *      recordCount: number
     * } & {[k in keyof T]: T[k] & (T[k])[]};
     */
    const query = () => {
        const v = cfStruct(nilTerminal, [], new Map(), nilTerminal); // Query<T> should be a built-in of somesort
        v.typeFlags |= TypeFlags.synthetic;
        return v;
    };

    return {
        _any,
        any,
        void_,
        string,
        number,
        boolean,
        struct,
        never,
        nil,
        query
    }
})();



// mostly just for exposition
const staticallyKnownCfTypes : readonly string[] = [
    "any",
    "array",
    "binary",
    "boolean",
    "date",
    "function",
    "guid",
    "numeric",
    "query",
    "string",
    "struct",
    "uuid",
    "void",
    "xml"
] as const;
staticallyKnownCfTypes; // "unused"

function stringifyDottedPath(dottedPath: DottedPath) {
    return [
        dottedPath.headKey.token.text,
        ...dottedPath.rest.map(e => e.key.token.text)
    ].join(".");

}

function typeFromJavaLikeTypename(dottedPath: DottedPath | null) : Type {
    if (dottedPath == null) return SyntheticType.any();
    if (dottedPath.rest.length > 0) {
        return SyntheticType.any();
    }
    return typeFromStringifiedJavaLikeTypename(stringifyDottedPath(dottedPath));
}

function typeFromStringifiedJavaLikeTypename(typename: string | null) {
    if (typename === null) return SyntheticType.any();
    switch (typename.toLowerCase()) {
        case "array": return cfArray(SyntheticType.any());
        case "boolean": return SyntheticType.boolean();
        case "function": return cfFunctionSignature("", [], SyntheticType.any());
        case "numeric": return SyntheticType.number();
        case "query": return SyntheticType.query();
        case "string": return SyntheticType.string();
        case "struct": return SyntheticType.struct();
        case "void": return SyntheticType.void_();
        default: return SyntheticType.any();
    }
}

function typeFromAttribute(attrs: TagAttribute[], attrName: string) : Type {
    return typeFromStringifiedJavaLikeTypename(
        getTriviallyComputableString(getAttributeValue(attrs, attrName)) || null);
}

export function extractCfFunctionSignature(def: FunctionDefinition | ArrowFunctionDefinition) {
    let uiName : string;
    let returnType : Type;
    let paramTypes : cfFunctionSignatureParam[] = [];

    if (def.kind === NodeType.functionDefinition) {
        if (def.fromTag) {
            uiName       = getTriviallyComputableString(getAttributeValue((def.tagOrigin.startTag as CfTag.Common).attrs, "name")) || "<<ERROR>>";
            returnType = typeFromAttribute((def.tagOrigin.startTag as CfTag.Common).attrs, "returntype");
            paramTypes = extractTagFunctionParam(def.params);
        }
        else {
            uiName     = def.nameToken?.uiName || ""; // anonymous function is OK
            returnType = typeFromJavaLikeTypename(def.returnType);
            paramTypes = extractScriptFunctionParam(def.params);
        }
    }
    else {
        uiName = ""; // arrow function never has a name
        returnType = SyntheticType.any();
        paramTypes = extractScriptFunctionParam(def.params);
    }

    return cfFunctionSignature(uiName, paramTypes, returnType);
}

export function extractScriptFunctionParam(params: readonly Script.FunctionParameter[]) : cfFunctionSignatureParam[] {
    const result : cfFunctionSignatureParam[] = [];
    for (const param of params) {
        const type = typeFromJavaLikeTypename(param.javaLikeTypename);
        const required = param.requiredTerminal ? TypeFlags.required : TypeFlags.none;
        const spread = param.dotDotDot ? TypeFlags.spread : TypeFlags.none;
        const name = param.identifier.uiName || "<<ERROR>>";
        const defaultValue = null;
        result.push(cfFunctionSignatureParam(required | spread, type, name, defaultValue));
    }
    return result;
}

export function extractTagFunctionParam(params: readonly Tag.FunctionParameter[]) : cfFunctionSignatureParam[] {
    const result : cfFunctionSignatureParam[] = [];
    for (const param of params) {
        const type = typeFromAttribute((param.tagOrigin.startTag as CfTag.Common).attrs, "type");
        const required = getTriviallyComputableBoolean(getAttributeValue((param.tagOrigin.startTag as CfTag.Common).attrs, "required"))
            ? TypeFlags.required
            : TypeFlags.none;
        const spread = TypeFlags.none;
        const name = getTriviallyComputableString(getAttributeValue((param.tagOrigin.startTag as CfTag.Common).attrs, "name")) || "<<ERROR>>";
        const defaultValue = null;

        result.push(cfFunctionSignatureParam(required | spread, type, name, defaultValue));
    }
    return result;
}

export function hashType(type: Type) : string {
    switch (type.typeKind) {
        case TypeKind.any: return "any";
        case TypeKind.number: return "number";
        case TypeKind.string: return "string";
        case TypeKind.void: return "void";
        case TypeKind.never: return "never";
        case TypeKind.functionSignature:
            return type.uiName +
                "(" + type.params.map(param => param.canonicalName + ":" + hashType(param.type)).join(",") + ")" +
                "=>" + hashType(type.returns);
        case TypeKind.array: return "[" + hashType(type.T) + "]";
        case TypeKind.struct: return "{" + type.members.map(member => member.propertyName + ":" + hashType(member.type)).join(",") + "}";
    }
    throw "unhandled hash type";
}
