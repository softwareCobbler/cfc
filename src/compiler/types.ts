import { ArrowFunctionDefinition, CfTag, DottedPath, FunctionDefinition, NodeKind, Script, SourceFile, SymTabEntry, Tag, TagAttribute, } from "./node";
import { getAttributeValue, getTriviallyComputableString, Mutable } from "./utils";

let debugTypeModule = true;
debugTypeModule;

export const enum TypeFlags {
    none                            = 0,
    any                             = 0x00000001,
    void                            = 0x00000002,
    string                          = 0x00000004,
    numeric                         = 0x00000008,
    boolean                         = 0x00000010,
    nil                             = 0x00000020,
    never                           = 0x00000040,
    array                           = 0x00000080,
    tuple                           = 0x00000100,
    struct                          = 0x00000200,
    union                           = 0x00000400,
    intersection                    = 0x00000800,
    functionSignature               = 0x00001000,
    functionSignatureParam          = 0x00002000,
    typeConstructor                 = 0x00004000,
    typeConstructorInvocation       = 0x00008000,
    cachedTypeConstructorInvocation = 0x00010000,
    typeConstructorParam            = 0x00020000,
    typeId                          = 0x00040000,
    final                           = 0x00080000,
    synthetic                       = 0x00100000,
    containsUndefined               = 0x00200000,
    optional                        = 0x00400000,
    spread                          = 0x00800000,
    mappedType                      = 0x01000000,
    indexedType                     = 0x02000000,
    cfc                             = 0x04000000,
    derived                         = 0x08000000,
    remote                          = 0x10000000,
    public                          = 0x20000000,
    protected                       = 0x40000000,
    private                         = 0x80000000,
    end
}

// workaround for bitshift operator being 32-bit
const powersOf2 = (() => {
    const result : number[] = [ /* 1, 2, 4, 8, ...*/ ];
    for (let i = 0; i < 53; i++) { // 2**53 > Number.MAX_SAFE_INTEGER
        result.push(2 ** i);
    }
    return result;
})();

const TypeKindUiString : Record<TypeFlags, string> = {
    [TypeFlags.none]:                            "none",
    [TypeFlags.any]:                             "any",
    [TypeFlags.void]:                            "void",
    [TypeFlags.string]:                          "string",
    [TypeFlags.numeric]:                          "number",
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
    [TypeFlags.containsUndefined]:               "has-undefined",
    [TypeFlags.optional]:                        "optional",
    [TypeFlags.spread]:                          "spread",
    [TypeFlags.mappedType]:                      "mapped-type",
    [TypeFlags.indexedType]:                     "indexed-type",
    [TypeFlags.cfc]:                             "cfc",
    [TypeFlags.derived]:                         "derived",

    // access modifiers for cfc member functions
    [TypeFlags.remote]:                          "remote",
    [TypeFlags.public]:                          "public",
    [TypeFlags.protected]:                       "protected",
    [TypeFlags.private]:                         "private",

    [TypeFlags.end]:                             "<<end-sentinel>>",
};

function addDebugTypeInfo(type: _Type) {
    Object.defineProperty(type, "__debugTypeInfo", {
        get() {
            const result : string[] = [];
            for (let i = 0; powersOf2[i] && powersOf2[i] < TypeFlags.end; i++) {
                if (type.flags & powersOf2[i]) {
                    result.push(TypeKindUiString[powersOf2[i] as TypeFlags]);
                }
            }
            return result.join(",");
        }
    })
}

export interface _Type {
    readonly flags: TypeFlags,
    readonly name?: string,
    readonly canonicalType?: _Type,
    readonly types?: _Type[],
    readonly cfc?: SourceFile, // kludge for connecting a cfstruct that represents a cfc to its cfc
}

export function getCanonicalType(type: _Type) {
    while (true) {
        if (type.canonicalType && type === type.canonicalType) return type;
        else if (type.flags & TypeFlags.derived) type = type.canonicalType!;
        else if (!(type.flags & TypeFlags.derived)) return type;
        else break;
    }
    // this is an error case; I don't beleive we should ever get here; a type should always have a canonical type
    return SyntheticType.any;
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
    t = getCanonicalType(t);
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
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.tuple);
}

export interface cfStruct extends _Type {
    members: ReadonlyMap<string, SymTabEntry>,
}

export function cfStruct(members: ReadonlyMap<string, SymTabEntry>) : cfStruct {
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
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.struct);
}

export interface cfFunctionSignature extends _Type {
    uiName: string,
    canonicalName: string,
    params: cfFunctionSignatureParam[],
    returns: _Type,
    attrs: TagAttribute[]
}

export function cfFunctionSignature(uiName: string, params: cfFunctionSignatureParam[], returns: _Type, attrs: TagAttribute[]) : cfFunctionSignature {
    const type = {
        flags: TypeFlags.functionSignature,
        uiName,
        canonicalName: uiName.toLowerCase(),
        params,
        returns,
        attrs
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isFunctionSignature(t: _Type) : t is cfFunctionSignature {
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.functionSignature);
}

// names are part of the type signature because a caller can specify named arguments
export interface cfFunctionSignatureParam extends _Type {
    type: _Type,
    uiName: string,
    canonicalName: string,
}

export function cfFunctionSignatureParam(required: boolean, paramType: _Type, uiName: string) : cfFunctionSignatureParam {
    const optionalFlag = required ? TypeFlags.none : TypeFlags.optional;
    const type = {
        flags: TypeFlags.functionSignatureParam | optionalFlag,
        type: paramType,
        uiName,
        canonicalName: uiName.toLowerCase(),
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isFunctionSignatureParam(t: _Type) : t is cfFunctionSignatureParam {
    t = getCanonicalType(t);
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
    t = getCanonicalType(t);
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
    t = getCanonicalType(t);
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
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.typeConstructor);
}

export interface cfTypeConstructorParam extends _Type {
    //extendsType: Type | null,
    name: string,
    defaultType?: _Type
}

export function cfTypeConstructorParam(name: string, defaultType?: _Type) {
    const type : cfTypeConstructorParam = {
        flags: TypeFlags.typeConstructorParam,
        name
    }

    if (defaultType) {
        type.defaultType = defaultType;
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isTypeConstructorParam(t: _Type) : t is cfTypeConstructorParam {
    t = getCanonicalType(t);
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
    t = getCanonicalType(t);
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
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.intersection);
}

export interface cfUnion extends _Type {
    types: _Type[],
}

export function cfUnion(types: _Type[], flags: TypeFlags = TypeFlags.none) {
    const identityDedup = [...new Set(types)];

    if (identityDedup.length === 1) {
        return freshType(identityDedup[0], flags);
    }
    else {
        return createType({
            flags: TypeFlags.union | flags,
            types: [...identityDedup],
        });
    }
}

export function isUnion(t: _Type) : t is cfUnion {
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.union);
}

export interface cfMappedType extends _Type {
    keyBinding: cfTypeId,
    inKeyOf: cfTypeId,
    targetType: _Type
}

export function cfMappedType(keyBinding: cfTypeId, inKeyOf: cfTypeId, targetType: _Type) : cfMappedType {
    const t = {
        flags: TypeFlags.mappedType,
        keyBinding,
        inKeyOf,
        targetType
    }

    if (debugTypeModule) {
        addDebugTypeInfo(t);
    }

    return t;
}

export function isMappedType(t: _Type) : t is cfMappedType {
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.mappedType);
}

export interface cfIndexedType extends _Type {
    type: _Type,
    index: cfTypeId
}

export function cfIndexedType(type: _Type, index: cfTypeId) : cfIndexedType {
    const t = {
        flags: TypeFlags.indexedType,
        type,
        index
    }

    if (debugTypeModule) {
        addDebugTypeInfo(t);
    }
    return t;
}

export function isIndexedType(t: _Type) : t is cfIndexedType {
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.indexedType);
}

export function isCfc(t: _Type) : boolean {
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.cfc);
}

export const SyntheticType = (function() {
    const any : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.any,
    } as _Type;
    (any as Mutable<_Type>).canonicalType = any;

    const anyFunction = (() => {
        const spreadParam = cfFunctionSignatureParam(false, cfArray(any), "args");
        (spreadParam as Mutable<_Type>).flags |= TypeFlags.spread;
        return cfFunctionSignature("", [spreadParam], any, []);
    })();

    const void_ : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.void,
    } as _Type;
    (void_ as Mutable<_Type>).canonicalType = void_;

    const string : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.string,
    } as _Type;
    (string as Mutable<_Type>).canonicalType = string;


    const numeric : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.numeric | TypeFlags.string,
    } as _Type;
    (numeric as Mutable<_Type>).canonicalType = numeric;

    const boolean : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.boolean,
    } as _Type;
    (boolean as Mutable<_Type>).canonicalType = boolean;

    const nil : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.nil,
    } as _Type;
    (nil as Mutable<_Type>).canonicalType = nil;

    const struct = (membersMap: ReadonlyMap<string, SymTabEntry> = new Map()) => {
        const v = cfStruct(membersMap);
        (v.flags as TypeFlags) |= TypeFlags.synthetic;
        return v;
    }

    const emptyStruct = struct();

    const never : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.never
    } as _Type;
    (never as Mutable<_Type>).canonicalType = never;

    const results = {
        any: createType(any),
        void_: createType(void_),
        string: createType(string),
        numeric: createType(numeric),
        boolean: createType(boolean),
        nil: createType(nil),
        never: createType(never),
        struct, // this is a function
        anyFunction: anyFunction, // already created via call to cfFunctionSignature
        emptyStruct, // already created via a call to cfStruct()
    } as const;

    return results;
})();

function createType(type: _Type) {
    if (debugTypeModule) addDebugTypeInfo(type);
    return type;
}

function freshType(type: _Type, flags: TypeFlags) : _Type {
    if (flags === TypeFlags.none || type.flags === flags) return type;
    else {
        flags |= TypeFlags.derived;
        return createType({flags, canonicalType: type});
    }
}

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
        case "function": return SyntheticType.anyFunction;
        case "numeric": return SyntheticType.numeric;
        case "query": return SyntheticType.emptyStruct; // @fixme: have real query type
        case "string": return SyntheticType.string;
        case "struct": return SyntheticType.emptyStruct;
        case "void": return SyntheticType.void_;
        default: return SyntheticType.any; // @fixme: should be CFC<typename>, which will be resolved to a CFC during checking phase (could also be a java class...)
    }
}

function typeFromAttribute(attrs: TagAttribute[], attrName: string) : _Type {
    return typeFromStringifiedJavaLikeTypename(
        getTriviallyComputableString(getAttributeValue(attrs, attrName)) || null);
}

export function extractCfFunctionSignature(def: FunctionDefinition | ArrowFunctionDefinition, asDeclaration = false) {
    let uiName : string;
    let returnType : _Type;
    let paramTypes : cfFunctionSignatureParam[] = [];
    const attrs = def.kind === NodeKind.functionDefinition ? def.attrs : [];

    if (def.kind === NodeKind.functionDefinition) {
        if (def.fromTag) {
            uiName       = getTriviallyComputableString(getAttributeValue((def.tagOrigin.startTag as CfTag.Common).attrs, "name")) || "<<ERROR>>";
            returnType = typeFromAttribute((def.tagOrigin.startTag as CfTag.Common).attrs, "returntype");
            paramTypes = extractTagFunctionParams(def.params);
        }
        else {
            uiName = def.nameToken?.uiName || ""; // anonymous function is OK
            returnType = asDeclaration
                ? (def.returnTypeAnnotation || SyntheticType.any)
                : typeFromJavaLikeTypename(def.returnType);
            paramTypes = extractScriptFunctionParams(def.params, asDeclaration);
        }
    }
    else {
        uiName = ""; // arrow function never has a name
        returnType = SyntheticType.any;
        paramTypes = extractScriptFunctionParams(def.params);
    }

    return cfFunctionSignature(uiName, paramTypes, returnType, attrs);
}

function extractScriptFunctionParams(params: readonly Script.FunctionParameter[], asDeclaration = false) {
    const result : cfFunctionSignatureParam[] = [];
    for (const param of params) {
        const type = asDeclaration
            ? (param.type || SyntheticType.any)
            : typeFromJavaLikeTypename(param.javaLikeTypename);
        result.push(cfFunctionSignatureParam(param.required && !param.defaultValue, type, param.identifier.uiName || "<<ERROR>>"));
    }
    return result;
}

function extractTagFunctionParams(params: readonly Tag.FunctionParameter[]) : cfFunctionSignatureParam[] {
    const result : cfFunctionSignatureParam[] = [];
    for (const param of params) {
        const type = typeFromAttribute((param.tagOrigin.startTag as CfTag.Common).attrs, "type");
        const name = getTriviallyComputableString(getAttributeValue((param.tagOrigin.startTag as CfTag.Common).attrs, "name")) || "<<ERROR>>";
        result.push(cfFunctionSignatureParam(param.required && !param.defaultValue, type, name));
    }
    return result;
}

export function stringifyType(_type: _Type, _depth = 0) : string {
    return "x";
    /*
    if (depth > 2) return "<<TYPE-TOO-DEEP>>";
    if (type.flags & TypeFlags.any) return "any";
    if (type.flags & TypeFlags.void) return "void";
    if (type.flags & TypeFlags.numeric) return "numeric";
    if (type.flags & TypeFlags.string) return "string";
    if (type.flags & TypeFlags.boolean) return "boolean";
    if (isStruct(type)) {
        const builder = [];
        for (const [propName, {type: memberType}] of type.members) {
            builder.push(propName + ": " + stringifyType(memberType, depth+1));
        }
        const result = builder.join(", ");
        return "{" + result + "}";
    }
    if (isArray(type)) {
        return stringifyType(type.memberType, depth+1) + "[]";
    }
    if (isUnion(type) || isIntersection(type)) {
        const joiner = isUnion(type) ? " | " : " & ";
        const result = type.types.map(memberType => stringifyType(memberType, depth+1)).join(joiner);
        return result;
    }
    if (isFunctionSignature(type)) {
        const params = [];
        for (const param of type.params) {
            params.push(stringifyType(param, depth+1));
        }
        return "(" + params.join(", ") + ")" + " => " + stringifyType(type.returns, depth+1);
    }
    if (isFunctionSignatureParam(type)) {
        return (!(type.flags & TypeFlags.optional) ? "required " : "") + type.uiName + ": " + (type.flags & TypeFlags.spread ? "..." : "") + stringifyType(type.type, depth+1);
    }

    return "<<type>>";
    */
}