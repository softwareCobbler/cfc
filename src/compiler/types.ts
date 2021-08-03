import { ArrowFunctionDefinition, CfTag, DottedPath, FunctionDefinition, NodeType, Script, SymTabEntry, Tag, TagAttribute, } from "./node";
import { getAttributeValue, getTriviallyComputableString } from "./utils";

let debugTypeModule = true;
debugTypeModule;

export const enum TypeFlags {
    none                            = 0,
    any                             = 1 << 0,
    void                            = 1 << 1,
    string                          = 1 << 2,
    number                          = 1 << 3,
    boolean                         = 1 << 4,
    nil                             = 1 << 5,
    never                           = 1 << 6,
    array                           = 1 << 7,
    tuple                           = 1 << 8,
    struct                          = 1 << 9,
    union                           = 1 << 10,
    intersection                    = 1 << 11,
    functionSignature               = 1 << 12,
    functionSignatureParam          = 1 << 13,
    typeConstructor                 = 1 << 14,
    typeConstructorInvocation       = 1 << 15,
    cachedTypeConstructorInvocation = 1 << 16,
    typeConstructorParam            = 1 << 17,
    typeId                          = 1 << 18,
    final                           = 1 << 19,
    synthetic                       = 1 << 20,
    containsUndefined               = 1 << 21,
    optional                        = 1 << 22,
    spread                          = 1 << 23,
    end
}

const TypeKindUiString : Record<TypeFlags, string> = {
    [TypeFlags.none]:                            "none",
    [TypeFlags.any]:                             "any",
    [TypeFlags.void]:                            "void",
    [TypeFlags.string]:                          "string",
    [TypeFlags.number]:                          "number",
    [TypeFlags.boolean]:                         "boolean",
    [TypeFlags.nil]:                             "nil",
    [TypeFlags.array]:                           "array",
    [TypeFlags.tuple]:                           "tuple",
    [TypeFlags.struct]:                          "struct",
    [TypeFlags.union]:                           "union",
    [TypeFlags.intersection]:                    "intersection",
    [TypeFlags.functionSignature]:               "function-signature",  // (name: type, ...) => type
    [TypeFlags.functionSignatureParam]:          "function-signature-param",
    [TypeFlags.typeConstructorInvocation]:       "type-constructor-invocation",           // typename | typename<type-list> where `typename` is shorthand for `typename<>` with 0 args
    [TypeFlags.cachedTypeConstructorInvocation]: "cached-type-constructor-invocation",    // same as a type call but we can see that is cached; sort of a "type call closure" with inital args captured
    [TypeFlags.typeConstructor]:                 "type-constructor",    // type<type-list, ...> => type
    [TypeFlags.typeConstructorParam]:            "type-function-param", // type param in a type function
    [TypeFlags.typeId]:                          "type-id",             // name of non-builtin type, e.g, "T"
    [TypeFlags.never]:                           "never",
    [TypeFlags.final]:                           "final",
    [TypeFlags.synthetic]:                       "synthetic",
};

function addDebugTypeInfo(type: _Type) {
    Object.defineProperty(type, "__debugTypeInfo", {
        get() {
            const result : string[] = [];
            for (let i = 0; (1 << i) < TypeFlags.end; i++) {
                if (type.flags & (1 << i)) {
                    result.push(TypeKindUiString[1 << i]);
                }
            }
            return result.join(",");
        }
    })
}

export interface _Type {
    readonly flags: TypeFlags,
    readonly name?: string,
}

export interface cfArray extends _Type {
    memberType: _Type
}

export function cfArray(memberType: _Type) : cfArray {
    const type = {
        flags: TypeFlags.array,
        memberType
    };

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }

    return type;
}

export function isArray(t: _Type) : t is cfArray {
    return !!(t.flags & TypeFlags.array);
}

export interface cfTuple extends _Type {
    readonly memberTypes: readonly _Type[]
}

export function cfTuple(memberTypes: readonly _Type[]) : cfTuple {
    const type = {
        flags: TypeFlags.tuple,
        memberTypes
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }

    return type;
}

export function isTuple(t: _Type) : t is cfTuple {
    return !!(t.flags & TypeFlags.tuple);
}

export interface cfStruct extends _Type {
    members: Map<string, SymTabEntry>,
}

export function cfStruct(members: Map<string, SymTabEntry>) : cfStruct {
    const type = {
        flags: TypeFlags.struct,
        members,
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }

    return type;
}

export function isStruct(t: _Type) : t is cfStruct {
    return !!(t.flags & TypeFlags.struct);
}

export interface cfFunctionSignature extends _Type {
    uiName: string,
    canonicalName: string,
    params: cfFunctionSignatureParam[],
    returns: _Type
}

export function cfFunctionSignature(uiName: string, params: cfFunctionSignatureParam[], returns: _Type) : cfFunctionSignature {
    const type = {
        flags: TypeFlags.functionSignature,
        uiName,
        canonicalName: uiName.toLowerCase(),
        params,
        returns,
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isFunctionSignature(t: _Type) : t is cfFunctionSignature {
    return !!(t.flags & TypeFlags.functionSignature);
}

export interface cfFunctionSignatureParam extends _Type {
    type: _Type,
    uiName: string,
    canonicalName: string,
    defaultValue: _Type | null,
}

export function cfFunctionSignatureParam(paramType: _Type, uiName: string, defaultValue: _Type | null) : cfFunctionSignatureParam {
    const type = {
        flags: TypeFlags.functionSignatureParam,
        type: paramType,
        uiName,
        canonicalName: uiName.toLowerCase(),
        defaultValue
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isFunctionSignatureParam(t: _Type) : t is cfFunctionSignatureParam {
    return !!(t.flags & TypeFlags.functionSignatureParam);
}

export interface cfTypeConstructorInvocation extends _Type {
    left: _Type,
    args: _Type[]
}

export function cfTypeConstructorInvocation(left: _Type, args: _Type[]) : cfTypeConstructorInvocation {
    const type = {
        flags: TypeFlags.typeConstructorInvocation,
        left,
        args
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isTypeConstructorInvocation(t: _Type) : t is cfTypeConstructorInvocation {
    return !!(t.flags & TypeFlags.typeConstructorInvocation);
}

export interface cfCachedTypeConstructorInvocation extends _Type {
    left: cfTypeConstructor,
    args: _Type[],
}

export function cfCachedTypeConstructorInvocation(left: cfTypeConstructor, args: _Type[]) : cfCachedTypeConstructorInvocation {
    const type = {
        flags: TypeFlags.cachedTypeConstructorInvocation,
        left,
        args
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isCachedTypeConstructorInvocation(t: _Type) : t is cfCachedTypeConstructorInvocation {
    return !!(t.flags & TypeFlags.cachedTypeConstructorInvocation);
}

export interface cfTypeConstructor extends _Type {
    params: cfTypeConstructorParam[],
    capturedParams: Map<string, _Type>, // "in context Γ extended with (T0,...,Tn)"; so @type Foo = <T> => <U> => (can see T, U here)
    body: _Type,
}

export function cfTypeConstructor(params: cfTypeConstructorParam[], body: _Type) : cfTypeConstructor {
    const type = {
        flags: TypeFlags.typeConstructor,
        params,
        capturedParams: new Map<string, _Type>(),
        body
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isTypeConstructor(t: _Type) : t is cfTypeConstructor {
    return !!(t.flags & TypeFlags.typeConstructor);
}

export interface cfTypeConstructorParam extends _Type {
    //extendsType: Type | null,
    name: string
}

export function cfTypeConstructorParam(name: string) {
    const type = {
        flags: TypeFlags.typeConstructorParam,
        name
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isTypeConstructorParam(t: _Type) : t is cfTypeConstructorParam {
    return !!(t.flags & TypeFlags.typeConstructorParam);
}

export interface cfTypeId extends _Type {
    name: string
}

export function cfTypeId(name: string) : cfTypeId {
    const type = {
        flags: TypeFlags.typeId,
        name
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isTypeId(t: _Type) : t is cfTypeId {
    return !!(t.flags & TypeFlags.typeId);
}

export interface cfIntersection extends _Type {
    types: _Type[]
}

export function cfIntersection(...types: _Type[]) : cfIntersection {
    const type = {
        flags: TypeFlags.intersection,
        types
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isIntersection(t: _Type) : t is cfIntersection {
    return !!(t.flags & TypeFlags.intersection);
}

export interface cfUnion extends _Type {
    types: _Type[],
}

export function cfUnion(...types: _Type[]) {
    const type = {
        flags: TypeFlags.union,
        types
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isUnion(t: _Type) : t is cfUnion {
    return !!(t.flags & TypeFlags.union);
}

export const SyntheticType = (function() {
    const any : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.any,
    }

    const void_ : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.void,
    }

    const string : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.string,
    }

    const number : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.number,
    }

    const boolean : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.boolean,
    }

    const nil : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.nil,
    }

    const struct = (membersMap: Map<string, SymTabEntry> = new Map()) => {
        const v = cfStruct(membersMap);
        (v.flags as TypeFlags) |= TypeFlags.synthetic;
        return v;
    }

    const emptyStruct = struct();

    const never : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.never
    }

    /**
     * goal would be something like
     * type Query = <T extends struct> => {
     *      recordCount: number
     * } & {[k in keyof T]: T[k] & (T[k])[]};
     */
    /*const query = () => {
        const v = cfStruct(nilTerminal, [], new Map(), nilTerminal); // Query<T> should be a built-in of somesort
        v.typeFlags |= TypeFlags.synthetic;
        return v;
    };*/

    const results = {
        any,
        void_,
        string,
        number,
        boolean,
        nil,
        struct,
        emptyStruct,
        never
    } as const;

    // struct is a function; and empty struct was created through a factory so it already has debug info on it
    // the remainder need debug info explicitly added
    const markDebug : (keyof typeof results)[] = ["any", "void_", "string", "number", "boolean", "nil", "never"];

    if (debugTypeModule) {
        for (const key of markDebug) {
            addDebugTypeInfo(results[key] as _Type);
        }
    }

    return results;
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

function typeFromJavaLikeTypename(dottedPath: DottedPath | null) : _Type {
    if (dottedPath == null) return SyntheticType.any;
    if (dottedPath.rest.length > 0) {
        return SyntheticType.any;
    }
    return typeFromStringifiedJavaLikeTypename(stringifyDottedPath(dottedPath));
}

function typeFromStringifiedJavaLikeTypename(typename: string | null) : _Type {
    if (typename === null) return SyntheticType.any;
    switch (typename.toLowerCase()) {
        case "array": return cfArray(SyntheticType.any);
        case "boolean": return SyntheticType.boolean;
        case "function": return cfFunctionSignature("", [], SyntheticType.any);
        case "numeric": return SyntheticType.number;
        case "query": return SyntheticType.emptyStruct; // @fixme: have real query type
        case "string": return SyntheticType.string;
        case "struct": return SyntheticType.emptyStruct;
        case "void": return SyntheticType.void_;
        default: return SyntheticType.any;
    }
}

function typeFromAttribute(attrs: TagAttribute[], attrName: string) : _Type {
    return typeFromStringifiedJavaLikeTypename(
        getTriviallyComputableString(getAttributeValue(attrs, attrName)) || null);
}

export function extractCfFunctionSignature(def: FunctionDefinition | ArrowFunctionDefinition) {
    let uiName : string;
    let returnType : _Type;
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
        returnType = SyntheticType.any;
        paramTypes = extractScriptFunctionParam(def.params);
    }

    return cfFunctionSignature(uiName, paramTypes, returnType);
}

export function extractScriptFunctionParam(params: readonly Script.FunctionParameter[]) : cfFunctionSignatureParam[] {
    const result : cfFunctionSignatureParam[] = [];
    for (const param of params) {
        const type = typeFromJavaLikeTypename(param.javaLikeTypename);
        //const required = param.requiredTerminal ? TypeFlags.none : TypeFlags.optional;
        //const spread = param.dotDotDot ? TypeFlags.spread : TypeFlags.none;
        const name = param.identifier.uiName || "<<ERROR>>";
        const defaultValue = null;
        result.push(cfFunctionSignatureParam(type, name, defaultValue));
    }
    return result;
}

export function extractTagFunctionParam(params: readonly Tag.FunctionParameter[]) : cfFunctionSignatureParam[] {
    const result : cfFunctionSignatureParam[] = [];
    for (const param of params) {
        const type = typeFromAttribute((param.tagOrigin.startTag as CfTag.Common).attrs, "type");
        /*const required = getTriviallyComputableBoolean(getAttributeValue((param.tagOrigin.startTag as CfTag.Common).attrs, "required"))
            ? TypeFlags.required
            : TypeFlags.none;
        const spread = TypeFlags.none;*/
        const name = getTriviallyComputableString(getAttributeValue((param.tagOrigin.startTag as CfTag.Common).attrs, "name")) || "<<ERROR>>";
        const defaultValue = null;

        result.push(cfFunctionSignatureParam(type, name, defaultValue));
    }
    return result;
}

/*
export function hashType(type: Type) : string {
    switch (type.typeKind) {
        case TypeKind.any: return "any";
        case TypeKind.number: return "number";
        case TypeKind.string: return "string";
        case TypeKind.void: return "void";
        case TypeKind.never: return "never";
        case TypeKind.boolean: return "boolean";
        case TypeKind.nil: return "nil";
        case TypeKind.typeId: return type.terminal.token.text;
        case TypeKind.functionSignature:
            return type.uiName +
                "(" + type.params.map(param => param.canonicalName + ":" + hashType(param.type)).join(",") + ")" +
                "=>" + hashType(type.returns);
        case TypeKind.array: return "[" + hashType(type.T) + "]";
        case TypeKind.struct: return "{" + type.members.map(member => member.propertyName + ":" + hashType(member.type)).join(",") + "}";
        case TypeKind.union: {
            const op = type.typeKind === TypeKind.union ? "|" : "&";
            return type.members.length > 0 ? type.members.map(type => hashType(type)).join(op) : "never";
        }
        /*default:
            ((_: never) => { throw "" })(type);* /
    }
    throw "unhandled hash type " + type.__debug_kind || "";
}
*/