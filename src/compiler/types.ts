import type { ArrowFunctionDefinition, CfTag, DottedPath, FunctionDefinition, InterpolatedStringLiteral, Script, SourceFile, SymbolTable, SymTabEntry, Tag, TagAttribute } from "./node";
import { NodeKind } from "./node";
import { exhaustiveCaseGuard, getAttributeValue, getTriviallyComputableString, Mutable } from "./utils";
import * as path from "path"; // !! for stringifying CFC types...do we really want this dependency here?
import { ComponentSpecifier } from "./project";

let debugTypeModule = false;

export function setDebug() : void { // can't unset after setting it per program run
    if (debugTypeModule) {
        return;
    }
    debugTypeModule = true;
    for (const key of Object.keys(BuiltinType) as (keyof typeof BuiltinType)[]) {
        addDebugTypeInfo(BuiltinType[key])
    }
}

export type Type =
    | cfAny
    | cfVoid
    | cfString
    | cfNumeric
    | cfBoolean
    | cfNull
    | cfUndefined
    | cfNever
    | cfArray
    | cfStructLike
    | cfUnion
    | cfArray
    | cfIntersection
    | cfFunctionSignature
    | cfFunctionSignatureParam
    | cfFunctionOverloadSet
    | cfGenericFunctionSignature
    | cfTypeConstructor
    | cfTypeConstructorParam
    | cfTypeConstructorInvocation
    | cfTypeId
    | cfLiteralType
    | cfMappedType
    | cfIndexedType
    | CfcLookup
    | cfKeyof
    | cfInterpolatedString
    | cfConditionalType
    | cfTuple
    
export const enum TypeKind {
    any,
    void,
    string,
    numeric,
    boolean,
    null,
    undefined,
    never,
    struct,
    interface,
    cfc,
    symbolTableTypeWrapper,
    union,
    array,
    intersection,
    functionSignature,
    functionSignatureParam,
    functionOverloadSet,
    genericFunctionSignature,
    typeConstructor,
    typeConstructorParam,
    typeConstructorInvocation,
    typeId,
    literal,
    mappedType,
    indexedType,
    decorator,
    cfcLookup,
    keyof,
    interpolatedString,
    conditional,
    tuple
}

export const enum TypeFlags {
    none              = 0,
    spread            = 1 << 1,
    optional          = 1 << 2,
    final             = 1 << 3,
    remote            = 1 << 4,
    public            = 1 << 5,
    protected         = 1 << 6,
    private           = 1 << 7,
    containsUndefined = 1 << 8,
    inferenceTarget             = 1 << 9,
    end
}

const TypeKindUiString : Record<TypeKind, string> = {
    [TypeKind.any]:                       "any",
    [TypeKind.void]:                      "void",
    [TypeKind.string]:                    "string",
    [TypeKind.numeric]:                   "numeric",
    [TypeKind.boolean]:                   "boolean",
    [TypeKind.null]:                      "null",
    [TypeKind.undefined]:                 "undefined",
    [TypeKind.never]:                     "never",
    [TypeKind.struct]:                    "struct",
    [TypeKind.interface]:                 "interface",                                  
    [TypeKind.cfc]:                       "cfc",
    [TypeKind.symbolTableTypeWrapper]:    "symbolTableTypeWrapper",                        
    [TypeKind.union]:                     "union",
    [TypeKind.array]:                     "array",
    [TypeKind.intersection]:              "intersection",
    [TypeKind.functionSignature]:         "functionSignature",
    [TypeKind.functionSignatureParam]:    "functionSignatureParam",
    [TypeKind.functionOverloadSet]:       "functionOverloadSet",
    [TypeKind.genericFunctionSignature]:  "genericFunctionSignature",
    [TypeKind.cfcLookup]:                 "cfcLookup",
    [TypeKind.typeConstructor]:           "typeConstructor",
    [TypeKind.typeConstructorParam]:      "typeConstructorParam",
    [TypeKind.typeConstructorInvocation]: "typeConstructorInvocation",
    [TypeKind.typeId]:                    "typeId",
    [TypeKind.literal]:                   "literal",
    [TypeKind.mappedType]:                "mappedType",
    [TypeKind.indexedType]:               "indexedType",
    [TypeKind.decorator]:                 "decorator",
    [TypeKind.keyof]:                     "keyof",
    [TypeKind.interpolatedString]:        "interpolatedStringType",
    [TypeKind.conditional]:               "conditionalType",
    [TypeKind.tuple]:                     "tuple",
}

const TypeFlagsUiString : Record<TypeFlags, string> = {
    [TypeFlags.none]:                            "",
    [TypeFlags.final]:                           "final",
    [TypeFlags.optional]:                        "optional",
    [TypeFlags.spread]:                          "spread",

    // access modifiers for cfc member functions
    [TypeFlags.remote]:                          "remote",
    [TypeFlags.public]:                          "public",
    [TypeFlags.protected]:                       "protected",
    [TypeFlags.private]:                         "private",
    [TypeFlags.containsUndefined]:               "uninitialized",
    [TypeFlags.inferenceTarget]:                 "infer",
    [TypeFlags.end]:                             "",
};

function addDebugTypeInfo(type: Type) {
    Object.defineProperty(type, "__debugTypeInfo", {
        get() {
            const typeAsString = TypeKindUiString[type.kind];
            const flags : string[] = [];
            for (let i = 1; (1 << i) < TypeFlags.end; i++) {
                if (type.flags & (1 << i)) {
                    flags.push(TypeFlagsUiString[1 << i]);
                }
            }

            const flagsString = flags.length === 0 ? "(none)" : flags.join(",");
            return typeAsString + " // flags: " + flagsString;
        }
    })
}

export interface TypeBase {
    readonly kind: TypeKind,
    readonly flags: TypeFlags,

    readonly underlyingType?: Type,
    readonly capturedContext?: ReadonlyMap<string, Type>, // for type evaluation of typeconstructors, is this necessary?
    readonly concrete?: boolean
}

// readonly name?: string,
// readonly underlyingType?: _Type,
// readonly types?: readonly _Type[],
// readonly cfc?: Readonly<SourceFile>, // kludge for connecting a cfstruct that represents a cfc to its cfc
// readonly literalValue?: string | number | boolean,

// readonly memberType?: _Type,

export function getCanonicalType(type: TypeBase) {
    while (true) {
        if (type === undefined) debugger;
        if (type.underlyingType && type === type.underlyingType) return type;
        if (!type.underlyingType) return type;
        type = type.underlyingType;
    }
    // this is an error case; I don't beleive we should ever get here; a type should always have a canonical type
    return BuiltinType.any;
}

export function UninstantiatedArray(memberType: Type) : cfTypeConstructorInvocation {
    return TypeConstructorInvocation(BuiltinType.arrayInterfaceName, [memberType]);
}

export function isUninstantiatedArray(type: Type) : type is cfTypeConstructorInvocation {
    return type.kind === TypeKind.typeConstructorInvocation
        && type.left.kind === TypeKind.typeId
        && type.left.name === "Array";
}

interface cfArray extends TypeBase {
    readonly kind: TypeKind.array
    readonly memberType: Type, // the T in `T[]`, once resolved to a type
    readonly members: ReadonlyMap<string, Readonly<SymTabEntry>>, // struct-like but we don't consider as such during type tests
}

export function cfArray(memberType: Type, members: ReadonlyMap<string, Readonly<SymTabEntry>>) : cfArray {
    return createType({
        kind: TypeKind.array,
        flags: TypeFlags.none,
        memberType,
        members,
    })
}

export type cfStructLike = Struct | Interface | Cfc | SymbolTableTypeWrapper;

export function isStructLike(t: Type) : t is cfStructLike {
    switch (t.kind) {
        case TypeKind.struct:
        case TypeKind.interface:
        case TypeKind.cfc:
        case TypeKind.symbolTableTypeWrapper:
            return true;
        default:
            return false;
    }
}

export function isStructLikeOrArray(t: Type) : t is cfStructLike | cfArray {
    return isStructLike(t) || t.kind === TypeKind.array;
}

export type IndexSignature = {
    // {[K : I]: T}
    name: string,        // K
    indexType: TypeBase, // I
    type: TypeBase       // T
};

interface StructBase extends TypeBase {
    readonly members: ReadonlyMap<string, Readonly<SymTabEntry>>
}

export interface Struct extends StructBase {
    readonly kind: TypeKind.struct,
    readonly indexSignature?: IndexSignature,
}

export interface Interface extends StructBase {
    readonly kind: TypeKind.interface,
    readonly name: string,
    readonly indexSignature?: IndexSignature,
    readonly typeParams?: readonly cfTypeConstructorParam[],
}

export interface Cfc extends StructBase {
    readonly kind: TypeKind.cfc,
    readonly cfc: Readonly<SourceFile>,
    // fixme: this should be on SourceFile ?
    readonly interfaceExtension: Readonly<Interface> | undefined // extension of `this` via interface, mostly experimental
    readonly isCfInterface: boolean,
}

export interface SymbolTableTypeWrapper extends StructBase {
    readonly kind: TypeKind.symbolTableTypeWrapper,
    // @interface variables { key: string }
    // variables.a = 42;
    // function foo() { variables. } <-- symbol table may be extended with an interface
    // interfaces will be merged with all sibling and parent interfaces of the same name prior to their attachment here
    // since at time of attachment to the wrapper here the merging will have already taken place, there is exactly 0 or 1 of these
    readonly interfaceExtension?: Readonly<Interface>
}

export function Struct(members: ReadonlyMap<string, SymTabEntry>, indexSignature?: IndexSignature, instantiableSpreads?: readonly TypeBase[]) : Struct {
    const type : Struct = {
        kind: TypeKind.struct,
        flags: TypeFlags.none,
        members,
    } as const;

    if (indexSignature) {
        (type as Mutable<Struct>).indexSignature = indexSignature;
    }
    if (instantiableSpreads && instantiableSpreads.length > 0) {
        // @instantiableSpreads
        // (type as Mutable<cfStruct>).instantiableSpreads = instantiableSpreads;
    }

    return createType(type);
}

export function Interface(name: string, members: ReadonlyMap<string, SymTabEntry>, typeParams?: readonly cfTypeConstructorParam[], indexSignature?: IndexSignature, instantiableSpreads?: readonly TypeBase[]) : Interface {
    const type : Interface = {
        kind: TypeKind.interface,
        flags: TypeFlags.none,
        name,
        members,
    } as const;

    if (typeParams && typeParams.length > 0) {
        (type as Mutable<Interface>).typeParams = typeParams;
    }
    if (indexSignature) {
        (type as Mutable<Interface>).indexSignature = indexSignature;
    }

    if (instantiableSpreads && instantiableSpreads.length > 0) {
        // @instantiableSpreads
        // (type as Mutable<Interface>).instantiableSpreads = instantiableSpreads;
    }

    return createType(type);
}

// a cfc can serve as a type, where we interpret the "this" scope as the members of a struct
// we store a reference to the sourceFile, which is expected to remain unchanged over the life of the cfc as it is edited
// the underlying `containedScope.this` may change, which is OK, and necessitates the getter
// todo: what happens when the target source file is deleted or otherwise unloaded?
export function Cfc(sourceFile: Readonly<SourceFile>) : Cfc {
    const result : Cfc = {
        kind: TypeKind.cfc,
        flags: TypeFlags.none,
        // fixme: this should be able to return undefined if the sourceFile is destroyed
        get members() : ReadonlyMap<string, SymTabEntry> {
            return sourceFile.containedScope.this!
        },
        get interfaceExtension() : Readonly<Interface> | undefined {
            return sourceFile.containedScope.typeinfo.mergedInterfaces.get("this");
        },
        cfc: sourceFile,
        isCfInterface: false,
    } as const;

    return createType(result);
}

export function SymbolTableTypeWrapper(symbolTable: Readonly<SymbolTable>, interfaceExtension?: Readonly<Interface>) : SymbolTableTypeWrapper {
    const type : SymbolTableTypeWrapper = {
        kind: TypeKind.symbolTableTypeWrapper,
        flags: TypeFlags.none,
        members: symbolTable,
    } as const;

    if (interfaceExtension) {
        (type as Mutable<SymbolTableTypeWrapper>).interfaceExtension = interfaceExtension;
    }

    return createType(type);
}

interface cfFunctionSignatureBase extends TypeBase {
    readonly uiName: string,
    readonly canonicalName: string,
    readonly params: cfFunctionSignatureParam[],
    readonly returns: Type,
    readonly attrs: TagAttribute[]
}
export interface cfFunctionSignature extends cfFunctionSignatureBase {
    readonly kind: TypeKind.functionSignature,
}
export interface cfGenericFunctionSignature extends cfFunctionSignatureBase {
    readonly kind: TypeKind.genericFunctionSignature,
    readonly typeParams: Readonly<cfTypeConstructorParam>[]
}

export function cfFunctionSignature(uiName: string, params: cfFunctionSignatureParam[], returns: Type, attrs: TagAttribute[]) : cfFunctionSignature {
    const type = {
        kind: TypeKind.functionSignature,
        flags: TypeFlags.none,
        uiName,
        canonicalName: uiName.toLowerCase(),
        params,
        returns,
        attrs
    } as const;

    return createType(type);
}

export function cfGenericFunctionSignature(uiName: string, typeParams: cfTypeConstructorParam[], params: cfFunctionSignatureParam[], returns: Type, attrs: TagAttribute[]) : cfGenericFunctionSignature {
    const type = {
        kind: TypeKind.genericFunctionSignature,
        flags: TypeFlags.none,
        uiName,
        canonicalName: uiName.toLowerCase(),
        typeParams,
        params,
        returns,
        attrs
    } as const;

    return createType(type);
}

export interface cfFunctionOverloadSet extends TypeBase {
    readonly kind: TypeKind.functionOverloadSet,
    readonly uiName: string,
    readonly canonicalName: string,
    readonly overloads: {
        readonly params: Readonly<cfFunctionSignatureParam>[],
        readonly returns: Readonly<Type>
    }[],
    readonly attrs: readonly TagAttribute[],
}

export function cfFunctionOverloadSet(uiName: string, overloads: {params: cfFunctionSignatureParam[], returns: Type}[], attrs: TagAttribute[]) : cfFunctionOverloadSet {
    const overloadSet = {
        kind: TypeKind.functionOverloadSet,
        flags: TypeFlags.none,
        uiName,
        canonicalName: uiName.toLowerCase(),
        overloads,
        attrs
    } as const

    return overloadSet;
}

// is this spread safe ? 
export function unsafe__cfFunctionSignatureWithFreshReturnType(other: cfFunctionSignature, freshReturnType: Type) : cfFunctionSignature {
    return {...other, returns: freshReturnType};
}

// names are part of the type signature because a caller can specify named arguments
export interface cfFunctionSignatureParam extends TypeBase {
    readonly kind: TypeKind.functionSignatureParam,
    readonly paramType: Type,
    readonly uiName: string,
    readonly canonicalName: string,
}

export function cfFunctionSignatureParam(required: boolean, paramType: Type, uiName: string, spread = false) : cfFunctionSignatureParam {
    const optionalFlag = required ? TypeFlags.none : TypeFlags.optional;
    const spreadFlag = spread ? TypeFlags.spread : TypeFlags.none;
    const type = {
        kind: TypeKind.functionSignatureParam,
        flags: optionalFlag | spreadFlag,
        paramType,
        uiName,
        canonicalName: uiName.toLowerCase(),
    } as const;

    return createType(type);
}
export interface cfTypeConstructorInvocation extends TypeBase {
    readonly kind: TypeKind.typeConstructorInvocation
    readonly left: Type,
    readonly args: Type[]
}

export function TypeConstructorInvocation(left: Type, args: Type[]) : cfTypeConstructorInvocation {
    const type = {
        kind: TypeKind.typeConstructorInvocation,
        flags: TypeFlags.none,
        left,
        args
    } as const;

    return createType(type);
}

export interface cfTypeConstructor extends TypeBase {
    readonly kind: TypeKind.typeConstructor,
    readonly params: readonly cfTypeConstructorParam[],
    readonly capturedParams: ReadonlyMap<string, Type>,
    readonly body: Type,
}

export function TypeConstructor(params: readonly cfTypeConstructorParam[], body: Type) : cfTypeConstructor {
    const type : cfTypeConstructor = {
        kind: TypeKind.typeConstructor,
        flags: TypeFlags.none,
        params,
        capturedParams: new Map<string, Type>(),
        body
    } as const;

    return createType(type);
}

export interface CfcLookup extends TypeBase {
    readonly kind: TypeKind.cfcLookup,
    readonly cfcName: cfLiteralType,
    readonly explicitSpecifier?: ComponentSpecifier // takes precedence over the `cfcName` if this exists
}

export function CfcLookup(param: string | cfLiteralType, explicitSpecifier?: ComponentSpecifier) : CfcLookup {
    const type : CfcLookup= {
        kind: TypeKind.cfcLookup,
        flags: TypeFlags.none,
        cfcName: typeof param === "string" ? createLiteralType(param) : param
    };

    if (explicitSpecifier) {
        (type as Mutable<CfcLookup>).explicitSpecifier = explicitSpecifier;
    }

    return createType(type);
}

export interface cfTypeConstructorParam extends TypeBase {
    kind: TypeKind.typeConstructorParam,
    name: string,
    defaultType?: Type // 'U' in <T = U>
    extends?: Type
}

export function TypeConstructorParam(name: string, defaultType?: Type, extends_?: Type) {
    const type : cfTypeConstructorParam = {
        kind: TypeKind.typeConstructorParam,
        flags: TypeFlags.none,
        name
    }

    if (defaultType) {
        type.defaultType = defaultType;
    }

    if (extends_) {
        type.extends = extends_;
    }

    return createType(type);
}

export interface cfTypeId extends TypeBase {
    readonly kind: TypeKind.typeId,
    readonly name: string,
    readonly indexChain?: readonly (cfLiteralType | cfTypeId)[]
}

export function cfTypeId(name: string, indexChain?: (cfLiteralType | cfTypeId)[]) : cfTypeId {
    const type : cfTypeId = {
        kind: TypeKind.typeId,
        flags: TypeFlags.none,
        name
    } as const;

    if (indexChain?.length) {
        (type as Mutable<cfTypeId>).indexChain = indexChain;
    }

    return createType(type);
}

/**
 * an inference target is like `A extends "foo#infer bar#"`
 * it seems that in `infer FOO` foo is always exactly a type id,
 * so this just returns a type id with the inferenceTarget flag set
 */
export function cfInferenceTarget(name: string) : cfTypeId {
    const type : cfTypeId = {
        kind: TypeKind.typeId,
        flags: TypeFlags.inferenceTarget,
        name
    } as const;

    return createType(type);
}

export interface cfTuple extends TypeBase {
    readonly kind: TypeKind.tuple,
    readonly elements: readonly Readonly<SymTabEntry>[]
}

export function cfTuple(es: SymTabEntry[]) {
    const type : cfTuple = {
        kind: TypeKind.tuple,
        flags: TypeFlags.none,
        elements: es
    }

    return createType(type);
}

export interface cfIntersection extends TypeBase {
    readonly kind: TypeKind.intersection,
    readonly types: readonly Type[]
}

export function cfIntersection(...types: Type[]) : cfIntersection {
    const type = {
        kind: TypeKind.intersection,
        flags: TypeFlags.none,
        types
    } as const;

    return createType(type);
}

export interface cfUnion extends TypeBase {
    readonly kind: TypeKind.union,
    readonly types: readonly Type[],
}

export function cfUnion(types: ReadonlySet<Type>, flags: TypeFlags = TypeFlags.none) : Type {
    if  (types.size === 1) {
        return [...types][0];
    }
    else {
        const workingTypes : Type[] = [];
        const result = new Set<Type>();
        for (const toplevelType of types) {
            workingTypes.push(toplevelType);
            while (workingTypes.length > 0) {
                const type = workingTypes.shift()!;
                if (type.kind === TypeKind.union) {
                    workingTypes.push(...type.types)
                }
                else {
                    result.add(type);
                }
            }
        }

        const union = {
            kind: TypeKind.union,
            flags: flags,
            types: [...result],
        } as cfUnion;

        return createType(union);
    }
}

export interface cfKeyof extends TypeBase {
    readonly kind: TypeKind.keyof,
    readonly operand: Type,
    // for comparison ease, it's not Set<cfLiteralType>
    // we could maybe do that if we had universally intern'd literal string types
    // but as it is now, two literal types with the same literal value are not guaranteed to be object-identity-equal
    readonly keyNames: Set<string>
}

export function freshKeyof(operand: Type) {
    const t : cfKeyof = {
        kind: TypeKind.keyof,
        flags: TypeFlags.none,
        operand,
        keyNames: new Set(),
        concrete: false
    }
    return createType(t);
}

export function cfKeyof(operand: Type, keyNames: Set<string>) : cfKeyof {
    const t : cfKeyof = {
        kind: TypeKind.keyof,
        flags: TypeFlags.none,
        operand,
        keyNames: keyNames,
        concrete: true
    }
    return createType(t);
}

export interface cfMappedType extends TypeBase {
    readonly kind: TypeKind.mappedType,
    readonly keyBinding: cfTypeId,
    readonly inKeyOf: cfTypeId,
    readonly targetType: TypeBase
}

export function cfMappedType(keyBinding: cfTypeId, inKeyOf: cfTypeId, targetType: TypeBase) : cfMappedType {
    const t = {
        kind: TypeKind.mappedType,
        flags: TypeFlags.none,
        keyBinding,
        inKeyOf,
        targetType
    } as const;

    return createType(t);
}

export interface cfIndexedType extends TypeBase {
    readonly kind: TypeKind.indexedType,
    readonly type: Type,
    readonly index: cfTypeId
}

export function cfIndexedType(type: Type, index: cfTypeId) : cfIndexedType {
    const t = {
        kind: TypeKind.indexedType,
        flags: TypeFlags.none,
        type,
        index
    } as const;

    return createType(t);
}

export interface cfInterpolatedString extends TypeBase {
    readonly kind: TypeKind.interpolatedString,
    readonly expr: InterpolatedStringLiteral,
}

export function cfInterpolatedString(expr: InterpolatedStringLiteral, isInferenceTarget = false): cfInterpolatedString{
    const t : cfInterpolatedString = {
        kind: TypeKind.interpolatedString,
        flags: isInferenceTarget ? TypeFlags.inferenceTarget : TypeFlags.none,
        expr
    }

    return createType(t);
}

export interface cfConditionalType extends TypeBase {
    readonly kind: TypeKind.conditional,
    readonly typeId: cfTypeId,
    readonly extends: Type,
    readonly consequent: Type,
    readonly alternative: Type
}

export function cfConditionalType(typeId: cfTypeId, extends_: Type, consequent: Type, alternative: Type) : cfConditionalType {
    const t = {
        kind: TypeKind.conditional,
        flags: TypeFlags.none,
        typeId,
        extends: extends_,
        consequent,
        alternative
    } as const;

    return createType(t);
}

export interface cfAny extends TypeBase {
    readonly kind: TypeKind.any
}

export interface cfVoid extends TypeBase {
    readonly kind: TypeKind.void
}

export interface cfNull extends TypeBase {
    readonly kind: TypeKind.null
}

export interface cfUndefined extends TypeBase {
    readonly kind: TypeKind.undefined
}

export interface cfString extends TypeBase {
    readonly kind: TypeKind.string
}

export interface cfNumeric extends TypeBase {
    readonly kind: TypeKind.numeric
}

export interface cfBoolean extends TypeBase {
    readonly kind: TypeKind.boolean
}

export interface cfNever extends TypeBase {
    readonly kind: TypeKind.never
}

export const BuiltinType = (function() {
    const any : cfAny  = {
        kind: TypeKind.any,
        flags: TypeFlags.none
    };

    const void_ : cfVoid = {
        kind: TypeKind.void,
        flags: TypeFlags.none,
    };

    const null_ : cfNull = {
        kind: TypeKind.null,
        flags: TypeFlags.none,
    };

    const undefined_ : cfUndefined = {
        kind: TypeKind.undefined,
        flags: TypeFlags.containsUndefined,
    }

    const string : cfString = {
        kind: TypeKind.string,
        flags: TypeFlags.none,
    };

    const numeric : cfNumeric = {
        kind: TypeKind.numeric,
        flags: TypeFlags.none,
    };

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

    const arrayInterfaceName = cfTypeId("Array");

    const anyFunction = (() => {
        // the spread param type should be ...any[], 
        // but it's unclear when we should instantiate the array type
        // instead, we'll treat it as a primitive, and "any function"
        // has assignability rules dictated in a manner similar to other primitives
        const spreadParam = cfFunctionSignatureParam(false, any, "args");
        (spreadParam as Mutable<TypeBase>).flags |= TypeFlags.spread;
        return cfFunctionSignature("", [spreadParam], any, []);
    })();

    const boolean : cfBoolean = {
        kind: TypeKind.boolean,
        flags: TypeFlags.none,
    };

    const _true : cfLiteralType = {
        kind: TypeKind.literal,
        flags: TypeFlags.none,
        literalValue: true,
        underlyingType: boolean,
    };

    const _false : cfLiteralType = {
        kind: TypeKind.literal,
        flags: TypeFlags.none,
        literalValue: false,
        underlyingType: boolean,
    };

    const never : cfNever = {
        kind: TypeKind.never,
        flags: TypeFlags.none,
    };

    const results = {
        any: createType(any),
        void: createType(void_),
        null: createType(null_),
        undefined: createType(undefined_),
        string: createType(string),
        numeric: createType(numeric),
        boolean: createType(boolean),
        true: createType(_true),
        false: createType(_false),
        never: createType(never),
        EmptyInterface: Interface("", new Map()),
        anyFunction: anyFunction, // already created via call to cfFunctionSignature
        arrayInterfaceName: createType(arrayInterfaceName),
    } as const;

    return results;
})();

export function createType<T extends Type>(type: T) : T {
    if (debugTypeModule) {
        addDebugTypeInfo(type);
    }

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

export function typeFromJavaLikeTypename(dottedPath: string | DottedPath | null) : Type {
    if (dottedPath === null) return BuiltinType.any;
    else if (typeof dottedPath === "string") return typeFromStringifiedJavaLikeTypename(dottedPath);
    return typeFromStringifiedJavaLikeTypename(stringifyDottedPath(dottedPath));
}

function typeFromStringifiedJavaLikeTypename(typename: string | null) : Type {
    if (typename === null) return BuiltinType.any; // function foo(bar) {} -- bar is any; var foo = (bar) => 42; -- bar is any
    switch (typename.toLowerCase()) {
        case "array": return UninstantiatedArray(BuiltinType.any);
        case "boolean": return BuiltinType.boolean;
        case "function": return BuiltinType.anyFunction;
        case "numeric": return BuiltinType.numeric;
        case "query": return BuiltinType.EmptyInterface; // @fixme: have real query type
        case "string": return BuiltinType.string;
        case "struct": return BuiltinType.EmptyInterface;
        case "void": return BuiltinType.void;
        case "any": return BuiltinType.any;
        default: return CfcLookup(typename); // @fixme: do we need to store "origin" so we can issue diagnostics on failing lookups?
    }
}

function typeFromAttribute(attrs: TagAttribute[], attrName: string) : Type {
    return typeFromStringifiedJavaLikeTypename(
        getTriviallyComputableString(getAttributeValue(attrs, attrName)) || null);
}

export function extractCfFunctionSignature(def: FunctionDefinition | ArrowFunctionDefinition, asDeclaration = false) {
    let uiName : string;
    let returnType : Type;
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
                ? (def.returnTypeAnnotation || BuiltinType.any)
                : typeFromJavaLikeTypename(def.returnType);
            paramTypes = extractScriptFunctionParams(def.params, asDeclaration);
        }
    }
    else {
        uiName = ""; // arrow function never has a name
        returnType = BuiltinType.any;
        paramTypes = extractScriptFunctionParams(def.params);
    }

    return cfFunctionSignature(uiName, paramTypes, returnType, attrs);
}

function extractScriptFunctionParams(params: readonly Script.FunctionParameter[], asDeclaration = false) {
    const result : cfFunctionSignatureParam[] = [];
    for (const param of params) {
        const type = asDeclaration
            ? (param.type || BuiltinType.any)
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

export interface cfLiteralType extends TypeBase {
    readonly kind: TypeKind.literal,
    readonly literalValue: string | number | boolean,
    readonly underlyingType: cfString | cfNumeric | cfBoolean
}

export function createLiteralType(value: string | number) : cfLiteralType {
    let result : cfLiteralType;
    if (typeof value === "string") {
        result = {
            kind: TypeKind.literal,
            flags: TypeFlags.none,
            underlyingType: BuiltinType.string,
            literalValue: value
        };
    }
    else {
        result = {
            kind: TypeKind.literal,
            flags: TypeFlags.none,
            underlyingType: BuiltinType.numeric,
            literalValue: value,
        };
    }

    return createType(result);
}

export function stringifyType(type: Type) : string {
    return stringifyTypeWorker(type, 0);

    function stringifyTypeWorker(type: Type, depth: number) : string {
        if (depth > 4) return "<<TYPE-TOO-DEEP>>"; // need to also check going too wide in lists
        
        switch (type.kind) {
            case TypeKind.any: return "any";
            case TypeKind.array: {
                return stringifyTypeWorker(type.memberType, depth + 1) + "[]";
            }
            case TypeKind.boolean: return "boolean";
            case TypeKind.cfc: {
                const baseNameNoExt = path.parse(type.cfc.absPath).base.replace(/\.cfc$/i, "");
                return "cfc<" + baseNameNoExt + ">";
            }
            case TypeKind.cfcLookup: {
                return "cfc<" + type.cfcName + ">";
            }
            case TypeKind.functionOverloadSet: return "function overload set";
            case TypeKind.functionSignature: {
                const params = [];
                for (const param of type.params) {
                    params.push(stringifyTypeWorker(param, depth+1));
                }
                return "(" + params.join(", ") + ")" + " => " + stringifyTypeWorker(type.returns, depth+1);
            }
            case TypeKind.functionSignatureParam: {
                const spread = (type.flags & TypeFlags.spread) ? "..." : "";
                const name = (type.uiName || "<<unnamed>>");
                const typing = (type.flags & TypeFlags.optional) ? "?: " : ": ";
                const typeString = stringifyTypeWorker(type.paramType, depth+1);
                return spread + name + typing + typeString;
            }
            case TypeKind.genericFunctionSignature: return "generic function sig";
            case TypeKind.indexedType: return "indexed type";
            case TypeKind.interface: {
                if (type === BuiltinType.EmptyInterface) {
                    return "{}";
                }
                else {
                    return type.name;
                }
            }
            case TypeKind.intersection: return "intersection";
            case TypeKind.literal: {
                if (typeof type.literalValue === "string") {
                    return '"' + type.literalValue + '"';
                }
                return String(type.literalValue);
            }
            case TypeKind.mappedType: return "mapped type";
            case TypeKind.never: return "never";
            case TypeKind.null: return "null";
            case TypeKind.numeric: return "numeric";
            case TypeKind.string: return "string";
            case TypeKind.struct: {
                const builder = [];
                for (const [propName, member] of type.members) {
                    const type = member.effectivelyDeclaredType ?? member.lexicalType;
                    const colon = member.links?.optional ? "?: " : ": ";
                    if (!type) {
                        console.log(`[dev] no type for struct memmber ${propName}`);
                        continue;
                    }
                    builder.push(propName + colon + stringifyTypeWorker(type, depth+1));
                }
                const result = builder.join(", ");
                return "{" + result + "}";
            }
            case TypeKind.symbolTableTypeWrapper: return "<<scope>>";
            case TypeKind.typeConstructor: return "type constructor";
            case TypeKind.typeConstructorInvocation: return "type constructor invocation";
            case TypeKind.typeConstructorParam: return "type constructor param";
            case TypeKind.typeId: {
                if (type.flags & TypeFlags.inferenceTarget) {
                    // inference target will never have a suffix chain
                    return `infer ${type.name}`
                }
                else {
                    // need to show possible chain like a.b.c.
                    return type.name;
                }
            }
            case TypeKind.union: {
                const maxPrintSize = 100;
                const result : string[] = [];
                let remaining = maxPrintSize;
                for (let i = 0; i < type.types.length; i++) {
                    const memberType = type.types[i];
                    const s = stringifyTypeWorker(memberType, depth + 1);
                    const earlyExit = i > 0 && (s.length > remaining);
                    if (earlyExit) {
                        result.push("...and " + (type.types.length - i) + " more");
                        break;
                    }
                    else {
                        const sliced = s.slice(0, remaining);
                        remaining -= sliced.length;
                        result.push(sliced);
                    }
                }
                return result.join(" | ");
            }
            case TypeKind.void: return "void";
            case TypeKind.undefined: {
                return "undefined";
            }
            case TypeKind.keyof: {
                return "keyof " + stringifyTypeWorker(type.operand, depth + 1);
            }
            case TypeKind.interpolatedString: {
                return "<interpolated-string>";
            }
            case TypeKind.conditional: {
                return "<conditional-type>"
            }
            case TypeKind.tuple: {
                return "[...tuple]"
            }
            default: exhaustiveCaseGuard(type);
        }
    }
}

const primitiveOrder = (() => {
    const v = BuiltinType;
    return new Map<TypeBase, number>([
        [v.any, 1],
        [v.void, 2],
        [v.null, 3],
        [v.string, 4],
        [v.numeric, 5],
        [v.boolean, 6],
        [v.true, 7],
        [v.false, 8],
        [v.never, 9],
    ])
})();

// goal here is a total ordering for the entire set of types in the language,
// primarily to support de-duplicating union types (it's hard to maintain object identity when merging various types into a union,
// but we can sort the members and then compare if they are equal, if we have a well defined ordering)
export function structurallyCompareTypes(l: Type, r: Type) : -1 | 0 | 1 {
    function comparePrimitiveTypes(l: Type, r: Type) {
        const pl = primitiveOrder.get(l)!;
        const pr = primitiveOrder.get(r)!;
        return pl === pr ? 0 : pl < pr ? -1 : 1;
    }
    
    function isPrimitiveType(t: Type) {
        return primitiveOrder.has(t);
    }

    if (l === r) {
        return 0;
    }

    // we don't know what two "T"'s here represent, so they are not equal
    // which makes some sense, since T in context<0> and T in context<1> could be totally different
    // but after instantiation, we will know if (context<0>, instantiated<T>) === (context<1>, instantiated<T>)
    // so checking if T[] is the same as T[] doesn't make sense,
    // but once instantiated we can check if `{x:number}[]` is the same as `{x:number}[]`
    // same with type constructors, we don't know if they're equal until after (arbitrarily complicated) evaluation
    if (l.kind === TypeKind.typeId || r.kind === TypeKind.typeId) return -1; 
    if (l.kind === TypeKind.typeConstructor || r.kind === TypeKind.typeConstructor) return -1;
    if (l.kind === TypeKind.typeConstructorInvocation || r.kind === TypeKind.typeConstructorInvocation) return -1;
    if (l.kind === TypeKind.typeConstructorParam || r.kind === TypeKind.typeConstructorParam) return -1;

    if (l.kind === TypeKind.literal && r.kind === TypeKind.literal) {
        if (typeof l.literalValue === typeof r.literalValue) {
            return l.literalValue === r.literalValue
                ? 0
                : l.literalValue < r.literalValue
                ? -1
                : 1;
        }
        else return -1;
    }

    if (isPrimitiveType(l) && isPrimitiveType(r)) {
        return comparePrimitiveTypes(l, r);
    }

    if (isPrimitiveType(l) && !isPrimitiveType(r)) {
        return -1;
    }

    if (!isPrimitiveType(l) && isPrimitiveType(r)) {
        return 1;
    }

    if (isStructLike(l) && isStructLike(r)) {
        if (l.kind === TypeKind.cfc && r.kind === TypeKind.cfc) {
            return l.cfc.absPath < r.cfc.absPath
                ? -1
                : l.cfc.absPath > r.cfc.absPath
                ? 0
                : 1;
        }

        if (l.kind === TypeKind.cfc && r.kind !== TypeKind.cfc) return -1;
        if (l.kind !== TypeKind.cfc && r.kind == TypeKind.cfc) return 1;

        if (l.members.size < r.members.size) return -1;
        if (l.members.size > r.members.size) return 1;

        function sortStructMembersByName([k0, _v0]: [string, SymTabEntry], [k1, _v1]: [string, SymTabEntry]) {
            return k0 < k1 ? -1 : k0 > k1 ? 1 : 0;
        }

        // these are now guaranteed the same size
        const lmembers = [...l.members.entries()].sort(sortStructMembersByName);
        const rmembers = [...r.members.entries()].sort(sortStructMembersByName);

        for (let i = 0; i < lmembers.length; i++) {
            const [lname, {lexicalType: ltype}] = lmembers[i];
            const [rname, {lexicalType: rtype}] = rmembers[i];

            if (lname < rname) return -1;
            if (lname > rname) return 1;

            const recursiveCompare = structurallyCompareTypes(ltype!, rtype!);
            if (recursiveCompare !== 0) return recursiveCompare;
        }

        // (struct(x), struct(x))
        // (type(x), struct(x)) => -1
        // (struct(x), type(x)) => 1

        return 0;
    }

    if (l.kind === TypeKind.struct && r.kind !== TypeKind.struct) return -1;
    if (l.kind !== TypeKind.struct && r.kind === TypeKind.struct) return 1;

    if (l.kind === TypeKind.union && r.kind === TypeKind.union) {
        if (l.types.length < r.types.length) return -1;
        if (l.types.length > r.types.length) return 1;

        const lmembers = [...l.types].sort(structurallyCompareTypes);
        const rmembers = [...r.types].sort(structurallyCompareTypes);

        for (let i = 0; i < lmembers.length; i++) {
            const ltype = lmembers[i];
            const rtype = rmembers[i];
            const recursiveCompare = structurallyCompareTypes(ltype, rtype);
            if (recursiveCompare !== 0) return recursiveCompare;
        }

        return 0;
    }

    if (l.kind === TypeKind.union && r.kind !== TypeKind.union) return -1;
    if (l.kind !== TypeKind.union && r.kind === TypeKind.union) return 1;

    if (l.kind === TypeKind.functionSignature && r.kind === TypeKind.functionSignature) {
        // should we compare method names? doesn't seem right, we could be comparing arrow functions or etc.
        if (l.params.length < r.params.length) return -1;
        if (l.params.length > r.params.length) return 1;

        for (let i = 0; i < l.params.length; i++) {
            const lparam = l.params[i];
            const rparam = r.params[i];
            const recursiveCompare = structurallyCompareTypes(lparam, rparam);
            if (recursiveCompare !== 0) return recursiveCompare;
        }

        return structurallyCompareTypes(l.returns, r.returns);
    }

    if (l.kind === TypeKind.functionSignature && r.kind !== TypeKind.functionSignature) return -1;
    if (l.kind !== TypeKind.functionSignature && r.kind === TypeKind.functionSignature) return 1;

    if (l.kind === TypeKind.functionSignatureParam && r.kind === TypeKind.functionSignatureParam) {
        return structurallyCompareTypes(l.paramType, r.paramType);
    }

    if (l.kind === TypeKind.functionSignatureParam && r.kind !== TypeKind.functionSignatureParam) return -1;
    if (l.kind !== TypeKind.functionSignatureParam && r.kind === TypeKind.functionSignatureParam) return 1;

    if (l.kind === TypeKind.undefined && r.kind !== TypeKind.undefined) return -1;
    if (l.kind !== TypeKind.undefined && r.kind === TypeKind.undefined) return 1;

    if (l.kind === TypeKind.array && r.kind === TypeKind.array) return structurallyCompareTypes(l.memberType, r.memberType);
    if (l.kind === TypeKind.array && r.kind !== TypeKind.array) return -1;
    if (l.kind !== TypeKind.array && r.kind == TypeKind.array) return 1;

    if (debugTypeModule) {
        debugger;
    }

    throw "unhandled types in structurallyCompareTypes";
}
