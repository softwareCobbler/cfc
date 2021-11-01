import type { ArrowFunctionDefinition, CfTag, DottedPath, FunctionDefinition, Script, SourceFile, SymbolTable, SymTabEntry, Tag, TagAttribute } from "./node";
import { NodeKind } from "./node";
import { exhaustiveCaseGuard, getAttributeValue, getTriviallyComputableString, Mutable } from "./utils";
import * as path from "path"; // !! for stringifying CFC types...do we really want this dependency here?

let debugTypeModule = true;
debugTypeModule;

export const enum TypeFlags {
    none                            = 0,
    any                             = 0x00000001,
    void                            = 0x00000002,
    string                          = 0x00000004,
    numeric                         = 0x00000008,
    boolean                         = 0x00000010,
    null                            = 0x00000020,
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
    decorator                       = 0x04000000,
    derived                         = 0x08000000,
    remote                          = 0x10000000,
    public                          = 0x20000000,
    protected                       = 0x40000000,
    private                         = 0x80000000,
    end
}

// workaround for bitshift operator being 32-bit
// fixme: doesn't work - bitwise | and & are 32-bit, too
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
    [TypeFlags.null]:                            "null",
    [TypeFlags.string]:                          "string",
    [TypeFlags.numeric]:                          "number",
    [TypeFlags.boolean]:                         "boolean",
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
    [TypeFlags.typeConstructorParam]:            "type-constructor-param", // type param in a type function
    [TypeFlags.typeId]:                          "type-id",             // name of non-builtin type, e.g, "T"
    [TypeFlags.never]:                           "never",
    [TypeFlags.final]:                           "final",
    [TypeFlags.synthetic]:                       "synthetic",
    [TypeFlags.containsUndefined]:               "has-undefined",
    [TypeFlags.optional]:                        "optional",
    [TypeFlags.spread]:                          "spread",
    [TypeFlags.mappedType]:                      "mapped-type",
    [TypeFlags.indexedType]:                     "indexed-type",
    [TypeFlags.decorator]:                       "decorator",
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
            // structlike things have a few subtypes we'd like to indicate
            if (isStructLike(type)) {
                const val = type.structKind;
                switch (val) {
                    case StructKind.struct: break; // already "struct" by TypeKindUiString
                    case StructKind.cfcTypeWrapper: result.push("CfcWrapper"); break;
                    case StructKind.interface: result.push("Interface"); break;
                    case StructKind.symbolTableTypeWrapper: result.push("SymbolTableWrapper"); break;
                    default: exhaustiveCaseGuard(val);
                }
            }
            return result.join(",");
        }
    })
}

export interface _Type {
    readonly flags: TypeFlags,
    readonly name?: string,
    readonly underlyingType?: _Type,
    readonly types?: _Type[],
    readonly cfc?: Readonly<SourceFile>, // kludge for connecting a cfstruct that represents a cfc to its cfc
    readonly literalValue?: string | number,
    readonly capturedContext?: ReadonlyMap<string, _Type>,
    readonly memberType?: _Type,
}

export function getCanonicalType(type: _Type) {
    while (true) {
        if (type === undefined) debugger;
        if (type.underlyingType && type === type.underlyingType) return type;
        if (!type.underlyingType) return type;
        type = type.underlyingType;
    }
    // this is an error case; I don't beleive we should ever get here; a type should always have a canonical type
    return SyntheticType.any;
}

export interface UninstantiatedArray extends TypeConstructorInvocation {
    left: cfTypeId,
    args: [_Type],
}

const arrayTypeID = cfTypeId("Array");
export function UninstantiatedArray(memberType: _Type) : TypeConstructorInvocation {
    return TypeConstructorInvocation(arrayTypeID, [memberType]);
}

export function isArray(t: _Type) : t is UninstantiatedArray { // fixme: should be instantiated?
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.array);
}

interface InstantiatedArray extends _Type {
    memberType: _Type
}

export function isInstantiatedArray(t: _Type) : t is InstantiatedArray {
    return !!(t.flags & TypeFlags.struct) // instantiated type is a struct
        && !!(t.underlyingType && isStructLike(t.underlyingType) && t.underlyingType.structKind === StructKind.interface && t.underlyingType.name === "Array") // the directly underlying type is the Array interface
        //&& !!(getCanonicalType(t).flags & TypeFlags.array) -- if we have an Array interface instantiation, we know the canonical type is an
}

export function isUninstantiatedArray(t: _Type) : t is UninstantiatedArray {
    return isTypeConstructorInvocation(t) && isTypeId(t.left) && t.left.name === "Array";
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

export const enum StructKind { struct, interface, cfcTypeWrapper, symbolTableTypeWrapper };

export type Struct = DefaultStruct | Interface | CfcTypeWrapper | SymbolTableTypeWrapper;

export type IndexSignature = {name: string, indexType: _Type, type: _Type};

interface StructBase extends _Type {
    readonly structKind: StructKind,
    readonly members: ReadonlyMap<string, Readonly<SymTabEntry>>,
    readonly indexSignature?: IndexSignature,
    readonly instantiableSpreads?: readonly _Type[],
}

export interface DefaultStruct extends StructBase {
    readonly structKind: StructKind.struct,
}

export interface Interface extends StructBase {
    readonly structKind: StructKind.interface,
    readonly name: string,
    readonly typeParams?: readonly TypeConstructorParam[],
}

export interface CfcTypeWrapper extends StructBase {
    readonly structKind: StructKind.cfcTypeWrapper,
    readonly cfc: Readonly<SourceFile>,
    readonly interfaceExtension: Readonly<Interface> | undefined // extension of `this` via interface
}

export interface SymbolTableTypeWrapper extends StructBase {
    readonly structKind: StructKind.symbolTableTypeWrapper,
    // @interface variables { key: string }
    // variables.a = 42;
    // function foo() { variables. } <-- symbol table may be extended with an interface
    // interfaces will be merged with all sibling and parent interfaces of the same name prior to their attachment here
    // since at time of attachment to the wrapper here the merging will have already taken place, there is exactly 0 or 1 of these
    readonly interfaceExtension?: Readonly<Interface>
}

export function Struct(members: ReadonlyMap<string, SymTabEntry>, indexSignature?: IndexSignature, instantiableSpreads?: readonly _Type[]) : DefaultStruct {
    const type : DefaultStruct = {
        flags: TypeFlags.struct,
        structKind: StructKind.struct,
        members,
    }

    if (indexSignature) {
        (type as Mutable<Struct>).indexSignature = indexSignature;
    }
    if (instantiableSpreads && instantiableSpreads.length > 0) {
        (type as Mutable<Struct>).instantiableSpreads = instantiableSpreads;
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }

    return type;
}

export function Interface(name: string, members: ReadonlyMap<string, SymTabEntry>, typeParams?: readonly TypeConstructorParam[], indexSignature?: IndexSignature, instantiableSpreads?: readonly _Type[]) : Interface {
    const type : Interface = {
        flags: TypeFlags.struct,
        structKind: StructKind.interface,
        name,
        members,
    }

    if (typeParams && typeParams.length > 0) {
        (type as Mutable<Interface>).typeParams = typeParams;
    }
    if (indexSignature) {
        (type as Mutable<Interface>).indexSignature = indexSignature;
    }

    if (instantiableSpreads && instantiableSpreads.length > 0) {
        (type as Mutable<Interface>).instantiableSpreads = instantiableSpreads;
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }

    return type;
}

// a cfc can serve as a type, where we interpret the "this" scope as the members of a struct
// we store a reference to the sourceFile, which is expected to remain unchanged over the life of the cfc as it is edited
// the underlying `containedScope.this` may change, which is OK, and necessitates the getter
// todo: what happens when the target source file is deleted or otherwise unloaded?
export function CfcTypeWrapper(sourceFile: Readonly<SourceFile>) : CfcTypeWrapper {
    const result : CfcTypeWrapper = {
        flags: TypeFlags.struct,
        structKind: StructKind.cfcTypeWrapper,
        // fixme: this should be able to return undefined if the sourceFile is destroyed
        get members() : ReadonlyMap<string, SymTabEntry> {
            return sourceFile.containedScope.this!
        },
        get interfaceExtension() : Readonly<Interface> | undefined {
            return sourceFile.containedScope.typedefs.mergedInterfaces.get("this");
        },
        cfc: sourceFile,
    } as const;

    if (debugTypeModule) {
        addDebugTypeInfo(result);
    }

    return result;
}

export function SymbolTableTypeWrapper(symbolTable: Readonly<SymbolTable>, interfaceExtension?: Readonly<Interface>) : SymbolTableTypeWrapper {
    const type : SymbolTableTypeWrapper = {
        flags: TypeFlags.struct,
        structKind: StructKind.symbolTableTypeWrapper,
        members: symbolTable,
    };

    if (interfaceExtension) {
        (type as Mutable<SymbolTableTypeWrapper>).interfaceExtension = interfaceExtension;
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }

    return type;
}

export function isStructLike(t: _Type) : t is Struct {
    // @rmme 10/25/21 t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.struct);
}

// fixme: narrowing on `structKind` not working?
export function isInterface(t: _Type) : t is Interface {
    return isStructLike(t) && t.structKind === StructKind.interface;
}

export interface cfFunctionSignature extends _Type {
    readonly uiName: string,
    readonly canonicalName: string,
    readonly params: cfFunctionSignatureParam[],
    readonly returns: _Type,
    readonly attrs: TagAttribute[]
}

export interface cfFunctionOverloadSet extends _Type {
    readonly uiName: string,
    readonly canonicalName: string,
    readonly overloads: {params: readonly Readonly<cfFunctionSignatureParam>[], returns: Readonly<_Type>}[],
    readonly attrs: readonly TagAttribute[],
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

export function cfFunctionOverloadSet(uiName: string, overloads: {params: cfFunctionSignatureParam[], returns: _Type}[], attrs: TagAttribute[]) : cfFunctionOverloadSet {
    const type = {
        flags: TypeFlags.functionSignature, // fixme: we ran out of type flags, we need a way to distinguish this type from a function signature
        uiName,
        canonicalName: uiName.toLowerCase(),
        overloads,
        attrs
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

// is this spread safe ? 
export function unsafe__cfFunctionSignatureWithFreshReturnType(other: cfFunctionSignature, freshReturnType: _Type) : cfFunctionSignature {
    return {...other, returns: freshReturnType};
}

export function isFunctionSignature(t: _Type) : t is cfFunctionSignature {
    // t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.functionSignature);
}

export function isFunctionOverloadSet(t: _Type) : t is cfFunctionOverloadSet {
    // t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.functionSignature) && !!(t as cfFunctionOverloadSet).overloads;
}

export interface GenericFunctionSignature extends TypeConstructor{
    readonly body: cfFunctionSignature
}

export function isGenericFunctionSignature(t: _Type) : t is GenericFunctionSignature {
    return isTypeConstructor(t) && isFunctionSignature(t.body);
}

// names are part of the type signature because a caller can specify named arguments
export interface cfFunctionSignatureParam extends _Type {
    type: _Type,
    uiName: string,
    canonicalName: string,
}

export function cfFunctionSignatureParam(required: boolean, paramType: _Type, uiName: string, spread = false) : cfFunctionSignatureParam {
    const optionalFlag = required ? TypeFlags.none : TypeFlags.optional;
    const spreadFlag = spread ? TypeFlags.spread : TypeFlags.none;
    const type = {
        flags: TypeFlags.functionSignatureParam | optionalFlag | spreadFlag,
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

export interface TypeConstructorInvocation extends _Type {
    left: _Type,
    args: _Type[]
}

export function TypeConstructorInvocation(left: _Type, args: _Type[]) : TypeConstructorInvocation {
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

export function isTypeConstructorInvocation(t: _Type) : t is TypeConstructorInvocation {
    // rmme 10/25/2021 t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.typeConstructorInvocation);
}

export interface cfCachedTypeConstructorInvocation extends _Type {
    left: TypeConstructor,
    args: _Type[],
}

export function cfCachedTypeConstructorInvocation(left: TypeConstructor, args: _Type[]) : cfCachedTypeConstructorInvocation {
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

export interface TypeConstructor extends _Type {
    readonly params: readonly TypeConstructorParam[],
    readonly capturedParams: ReadonlyMap<string, _Type>, // "in context Γ extended with (T0,...,Tn)"; so @type Foo = <T> => <U> => (can see T, U here)
    readonly body: _Type,
}

export function TypeConstructor(params: readonly TypeConstructorParam[], body: _Type) : TypeConstructor {
    const type : TypeConstructor = {
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

export type CfcLookupType = TypeConstructorInvocation & {params: [/*fixme: LiteralType*/ _Type], __tag: "CfcLookupType"};

export function CfcLookup(param: _Type | string) : CfcLookupType {
        if (typeof param === "string") {
            const paramAsLiteralStringType = createLiteralType(param);
            return TypeConstructorInvocation(SyntheticType.cfcLookupType, [paramAsLiteralStringType]) as CfcLookupType;
        }
        else {
            return TypeConstructorInvocation(SyntheticType.cfcLookupType, [param]) as CfcLookupType;
        }
}

export function isTypeConstructor(t: _Type) : t is TypeConstructor {
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.typeConstructor);
}

export function isCfcLookupType(t: _Type) : t is CfcLookupType {
    return isTypeConstructorInvocation(t) && t.left === SyntheticType.cfcLookupType;
}

export function getCfcLookupTarget(t: CfcLookupType) : string {
    return t.params[0].literalValue! as string;
}

export interface TypeConstructorParam extends _Type {
    //extendsType: Type | null,
    name: string,
    defaultType?: _Type
}

export function TypeConstructorParam(name: string, defaultType?: _Type) {
    const type : TypeConstructorParam = {
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

export function isTypeConstructorParam(t: _Type) : t is TypeConstructorParam {
    t = getCanonicalType(t);
    return !!(t.flags & TypeFlags.typeConstructorParam);
}

export interface cfTypeId extends _Type {
    readonly name: string,
    readonly indexChain?: readonly string[]
}

export function cfTypeId(name: string, indexChain?: string[]) : cfTypeId {
    const type : cfTypeId = {
        flags: TypeFlags.typeId,
        name
    }
    if (indexChain) {
        (type as Mutable<cfTypeId>).indexChain = indexChain;
    }

    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }
    
    return type;
}

export function isTypeId(t: _Type) : t is cfTypeId {
    //t = getCanonicalType(t);
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
    if  (types.length === 1) {
        return types[0];
    }
    else {
        return createType({
            flags: TypeFlags.union | flags,
            types: [...types],
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

export interface Decorator extends _Type {
    name: string
}

export function Decorator(name: string) : Decorator {
    return {
        flags: TypeFlags.decorator,
        name
    }
}

export function isDecorator(t: _Type) : t is Decorator {
    return !!(t.flags & TypeFlags.decorator);
}

export const SyntheticType = (function() {
    const any : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.any,
    } as _Type;
    (any as Mutable<_Type>).underlyingType = any;

    const void_ : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.void,
    } as _Type;
    (void_ as Mutable<_Type>).underlyingType = void_;

    const null_ : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.null,
    } as _Type;
    (null_ as Mutable<_Type>).underlyingType = null_;

    const string : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.string,
    } as _Type;
    (string as Mutable<_Type>).underlyingType = string;

    const numeric : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.numeric | TypeFlags.string,
    } as _Type;
    (numeric as Mutable<_Type>).underlyingType = numeric;

    // wip:
    // the array interface could maybe be a builtin?
    // instead and for now, we recognize `T[]` and `Array<T>` as being single argument type constructors,
    // with the constructor being the "Array" interface, which we assume is declared somewhere in a library file at time of lookup
    // if we had a single global "ArrayInterface", extending it via a library would have us lose the Object identity property,
    // or rather change it in every compilation unit
    // const ArrayInterface = Interface(
    //     "Array",
    //     new Map<string, SymTabEntry>(),
    //     [TypeConstructorParam("T", any)],
    //     {name: "index", indexType: numeric, type: any});

    // we need an instantiated any[]

    const anyFunction = (() => {
        const spreadParam = cfFunctionSignatureParam(false, /*should be the builtin instantiated any[]*/ any, "args");
        (spreadParam as Mutable<_Type>).flags |= TypeFlags.spread;
        return cfFunctionSignature("", [spreadParam], any, []);
    })();

    const boolean : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.boolean,
    } as _Type;
    (boolean as Mutable<_Type>).underlyingType = boolean;

    const struct = (membersMap: ReadonlyMap<string, SymTabEntry> = new Map()) => {
        const v = Struct(membersMap);
        (v.flags as TypeFlags) |= TypeFlags.synthetic;
        return v;
    }

    const never : _Type = {
        flags: TypeFlags.synthetic | TypeFlags.never
    } as _Type;
    (never as Mutable<_Type>).underlyingType = never;

    const results = {
        any: createType(any),
        void: createType(void_),
        null: createType(null_),
        string: createType(string),
        numeric: createType(numeric),
        boolean: createType(boolean),
        never: createType(never),
        struct, // this is a function
        anyFunction: anyFunction, // already created via call to cfFunctionSignature
        cfcLookupType: TypeConstructor([], any), // param and body are meaningless here, we just want a single type constructor that says "hey, lookup the name of this as a cfc as a type"
        EmptyInterface: Interface("", new Map()),
        //ArrayInterface: ArrayInterface,
    } as const;

    return results;
})();

export function createType(type: _Type) {
    if (debugTypeModule) addDebugTypeInfo(type);
    return type;
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

export function typeFromJavaLikeTypename(dottedPath: string | DottedPath | null) : _Type {
    if (dottedPath === null) return SyntheticType.any;
    else if (typeof dottedPath === "string") return typeFromStringifiedJavaLikeTypename(dottedPath);
    return typeFromStringifiedJavaLikeTypename(stringifyDottedPath(dottedPath));
}

function typeFromStringifiedJavaLikeTypename(typename: string | null) : _Type {
    if (typename === null) return SyntheticType.any; // function foo(bar) {} -- bar is any; var foo = (bar) => 42; -- bar is any
    switch (typename.toLowerCase()) {
        case "array": return UninstantiatedArray(SyntheticType.any);
        case "boolean": return SyntheticType.boolean;
        case "function": return SyntheticType.anyFunction;
        case "numeric": return SyntheticType.numeric;
        case "query": return SyntheticType.EmptyInterface; // @fixme: have real query type
        case "string": return SyntheticType.string;
        case "struct": return SyntheticType.EmptyInterface;
        case "void": return SyntheticType.void;
        case "any": return SyntheticType.any;
        default: return CfcLookup(typename); // @fixme: do we need to store "origin" so we can issue diagnostics on failing lookups?
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

export function createLiteralType(value: string | number) : _Type {
    let result : _Type;
    if (typeof value === "string") {
        result = {
            flags: TypeFlags.synthetic | TypeFlags.string,
            underlyingType: SyntheticType.string,
            literalValue: value
        };
    }
    else {
        result = {
            flags: TypeFlags.synthetic | TypeFlags.numeric,
            underlyingType: SyntheticType.numeric,
            literalValue: value,
        };
    }

    if (debugTypeModule) {
        addDebugTypeInfo(result);
    }

    return result;
}

// fixme: ran out of flags, need a better way to discriminate a literal type
const __literalTypeKey : keyof _Type = "literalValue";
export interface _LiteralType extends _Type {
    literalValue: string | number,
    underlyingType: _Type
}
export type LiteralType = _LiteralType;

export function isLiteralType(_type: _Type) : _type is LiteralType {
    return _type.hasOwnProperty(__literalTypeKey);
}

export function stringifyType(type: _Type, depth = 0) : string {
    if (depth > 4) return "<<TYPE-TOO-DEEP>>";
    if (type.flags & TypeFlags.any) return "any";
    if (type.flags & TypeFlags.void) return "void";
    if (type.flags & TypeFlags.numeric) return "numeric";
    if (type.flags & TypeFlags.string) return "string";
    if (type.flags & TypeFlags.boolean) return "boolean";
    if (type.flags & TypeFlags.never) return "never";
    if (isTypeId(type)) {
        return type.name;
    }
    if (isStructLike(type)) {
        if (isInstantiatedArray(type)) {
            return stringifyType(type.memberType, depth + 1) + "[]";
        }
        else if (type.structKind === StructKind.cfcTypeWrapper) {
            const baseNameNoExt = path.parse(type.cfc.absPath).base.replace(/\.cfc$/i, "");
            return "cfc<" + baseNameNoExt + ">";
        }
        else {
            const builder = [];
            for (const [propName, {type: memberType}] of type.members) {
                builder.push(propName + ": " + stringifyType(memberType, depth+1));
            }
            const result = builder.join(", ");
            return "{" + result + "}";
        }
    }
    if (isUnion(type)) {
        return "<<union>>";
    }
    if (isIntersection(type)) {
        return "<<intersection>>";
    }
    if (isFunctionSignature(type)) {
        if (isFunctionOverloadSet(type)) {
            return "<<function-overload-set>>";
        }
        else if (isGenericFunctionSignature(type)) {
            return "<<generic-function-sig>>";
        }
        else {
            const params = [];
            for (const param of type.params) {
                params.push(stringifyType(param, depth+1));
            }
            return "(" + params.join(", ") + ")" + " => " + stringifyType(type.returns, depth+1);
        }
    }
    if (isFunctionSignatureParam(type)) {
        const spread = (type.flags & TypeFlags.spread) ? "..." : "";
        const name = (type.uiName || "<<unnamed>>");
        const typing = (type.flags & TypeFlags.optional) ? "?: " : ": ";
        const typeString = stringifyType(type.type);
        return spread + name + typing + typeString;
    }

    return "<<complex-type-we-can't-serialize-yet>>";
}