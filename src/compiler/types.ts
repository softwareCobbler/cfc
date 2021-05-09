import { NodeBase, NodeType } from "./node";

let debugTypeModule = true;

const enum TypeKind {
    any,
    void,
    string,
    number,
    boolean,
    nil,
    array,
    tuple,
    struct,
    union,
    functionSignature,
    typeCall,
}

const TypeKindUiString : Record<TypeKind, string> = {
    [TypeKind.any]:               "any",
    [TypeKind.void]:               "void",
    [TypeKind.string]:            "string",
    [TypeKind.number]:            "number",
    [TypeKind.boolean]:           "boolean",
    [TypeKind.nil]:               "nil",
    [TypeKind.array]:             "array",
    [TypeKind.tuple]:             "tuple",
    [TypeKind.struct]:            "struct",
    [TypeKind.union]:             "union",
    [TypeKind.functionSignature]: "function-signature", // (name: type, ...) => type
    [TypeKind.typeCall]:          "type-call", // type<type-list, ...> => type
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

export type Type = cfAny | cfVoid | cfString | cfNumber | cfBoolean | cfNil | cfArray | cfTuple | cfStruct | cfFunctionSignature | cfTypeCall;

export interface cfAny extends TypeBase {
    typeKind: TypeKind.any;
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
    literal?: string,
}

export function cfString(literal?: string) : cfString {
    const v = TypeBase<cfString>(TypeKind.string);
    if (literal !== undefined) {
        v.literal = literal;
    }
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
    T: Map<string, Type>,
}

export function cfStruct(T: Map<string, Type>, stringIndex = false) : cfStruct {
    const v = TypeBase<cfStruct>(TypeKind.struct);
    v.T = T;
    v.stringIndex = stringIndex;
    return v;
}

export interface cfFunctionSignature extends TypeBase {
    typeKind: TypeKind.functionSignature,
    name: string,
    params: Type[],
    returns: Type
}

export function cfFunctionSignature(name: string, params: Type[], returns: Type) : cfFunctionSignature {
    const v = TypeBase<cfFunctionSignature>(TypeKind.functionSignature);
    v.name = name;
    v.params = params;
    v.returns = returns;
    return v;
}

export interface cfTypeCall extends TypeBase {
    typeKind: TypeKind.typeCall,
    left: Type,
    params: Type[]
    returnType: Type | null,
}

export function cfTypeCall(left: Type, params: Type[], returnType: Type | null) : cfTypeCall {
    const v = TypeBase<cfTypeCall>(TypeKind.typeCall);
    v.left = left;
    v.params = params;
    v.returnType = returnType;
    return v;
}