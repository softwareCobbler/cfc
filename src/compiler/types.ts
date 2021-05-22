import { FunctionParameter, NodeBase, NodeType, Terminal } from "./node";

let debugTypeModule = true;

export const enum TypeKind {
    any,
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
    typeConstructor,
    typeConstructorInvocation,
    cachedTypeConstructorInvocation,
    typeFunctionParam,
    typeId,
    deferred,
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
    [TypeKind.typeConstructorInvocation]:       "type-constructor-invocation",           // typename | typename<type-list> where `typename` is shorthand for `typename<>` with 0 args
    [TypeKind.cachedTypeConstructorInvocation]: "cached-type-constructor-invocation",    // same as a type call but we can see that is cached; sort of a "type call closure" with inital args captured
    [TypeKind.typeConstructor]:                 "type-constructor",    // type<type-list, ...> => type
    [TypeKind.typeFunctionParam]:               "type-function-param", // type param in a type function
    [TypeKind.typeId]:                          "type-id",             // name of non-builtin type, e.g, "T"
    [TypeKind.never]:                           "never",
    [TypeKind.deferred]:                        "deferred",
}

export const enum TypeFlags {
    none     = 0,
    optional = 1 << 1,
    const    = 1 << 2
}

if (TypeFlags.optional) {};

export interface TypeBase extends NodeBase {
    kind: NodeType.type,
    typeKind: TypeKind,
    typeFlags: TypeFlags,

    name?: string,
    __debug_kind?: string,
}

export function TypeBase<T extends Type>(typeKind: T["typeKind"]) : T {
    const result = NodeBase<Type>(NodeType.type);
    result.typeKind = typeKind;
    result.typeFlags = TypeFlags.none;

    if (debugTypeModule) {
        result.__debug_kind = TypeKindUiString[typeKind];
    }

    return result as T;
}

export type Type =
    | cfAny | cfVoid | cfString | cfNumber | cfBoolean | cfNil | cfArray
    | cfUnion | cfIntersection
    | cfTypeId | cfNever | cfDeferred
    | cfTuple | cfStruct | cfFunctionSignature
    | cfTypeConstructorInvocation | cfCachedTypeConstructorInvocation | cfTypeConstructor | cfTypeConstructorParam;

export interface cfAny extends TypeBase {
    typeKind: TypeKind.any;
    terminal: Terminal
}

export function cfAny() : cfAny {
    const v = TypeBase<cfAny>(TypeKind.any);
    return v;
}

export interface cfVoid extends TypeBase {
    typeKind: TypeKind.void;
}

export function cfVoid() : cfVoid {
    const v = TypeBase<cfVoid>(TypeKind.void);
    return v;
}

export interface cfString extends TypeBase {
    typeKind: TypeKind.string,
    literal: string | null,
}

export function cfString(literal?: string) : cfString {
    const v = TypeBase<cfString>(TypeKind.string);
    v.literal = literal ?? null;
    return v;
}

export interface cfNumber extends TypeBase {
    typeKind: TypeKind.number,
    literal?: number,
}

export function cfNumber(literal?: number) : cfNumber {
    const v = TypeBase<cfNumber>(TypeKind.number);
    if (literal !== undefined) {
        v.literal = literal;
    }
    return v;
}

export interface cfBoolean extends TypeBase {
    typeKind: TypeKind.boolean,
    literal?: boolean
}

export function cfBoolean(literal?: boolean) : cfBoolean {
    const v = TypeBase<cfBoolean>(TypeKind.boolean);
    if (literal !== undefined) {
        v.literal = literal;
    }
    return v;
}

export interface cfNil extends TypeBase {
    typeKind: TypeKind.nil
}

export function cfNil() : cfNil {
    const v = TypeBase<cfNil>(TypeKind.nil);
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
    stringIndex: boolean,
    members: Map<string, Type>,
}

export function cfStruct(T: Map<string, Type> = new Map(), stringIndex = false) : cfStruct {
    const v = TypeBase<cfStruct>(TypeKind.struct);
    v.members = T;
    v.stringIndex = stringIndex;
    return v;
}

export interface cfFunctionSignature extends TypeBase {
    typeKind: TypeKind.functionSignature,
    name: string,
    params: FunctionParameter[],
    returns: Type
}

export function cfFunctionSignature(name: string, params: FunctionParameter[], returns: Type) : cfFunctionSignature {
    const v = TypeBase<cfFunctionSignature>(TypeKind.functionSignature);
    v.name = name;
    v.params = params;
    v.returns = returns;
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
    name: string,
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
    name: string,
    extendsToken: Terminal | null,
    extendsType: Type | null,
    comma: Terminal | null
}

export function cfTypeConstructorParam(name: string, extendsToken: Terminal | null, extendsType: Type | null, comma: Terminal | null) {
    const v = TypeBase<cfTypeConstructorParam>(TypeKind.typeFunctionParam);
    v.name = name;
    v.extendsToken = extendsToken;
    v.extendsType = extendsType;
    v.comma = comma;
    return v;
}

export interface cfTypeId extends TypeBase {
    typeKind: TypeKind.typeId,
    name: string,
}

export function cfTypeId(name: string) : cfTypeId {
    const v = TypeBase<cfTypeId>(TypeKind.typeId);
    v.name = name;
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
    left: Type,
    right: Type,
}

export function cfUnion(left: Type, right: Type) {
    const v = TypeBase<cfUnion>(TypeKind.union);
    v.left = left;
    v.right = right;
    return v;
}

export interface cfNever extends TypeBase {
    typeKind: TypeKind.never,
}

export function cfNever() : cfNever {
    const v = TypeBase<cfNever>(TypeKind.never);
    return v;
}

export interface cfDeferred extends TypeBase {
    typeKind: TypeKind.deferred,
}

export function cfDeferred() {
    const v = TypeBase<cfDeferred>(TypeKind.deferred);
    return v;
}
