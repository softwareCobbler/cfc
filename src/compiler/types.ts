`

declare arrayFindNoCase(required array /*: []*/, required value /*: any*/, parallel /*: boolean*/, maxThreadCount /*: number*/): number;
declare arrayFindNoCase(required array /*: []*/, required callback /*: (v: any) => boolean*/): number;

function foo(bar /*: [number, string][]*/) {}

<cffunction name="foo">
    <cfargument name="bar" type:="[const number, const string][]">
</cffunction>

`

let debugTypeModule = true;

const enum TypeKind {
    any,
    string,
    number,
    boolean,
    nil,
    array,
    tuple,
    struct,
    union,
}

const TypeKindUiString : Record<TypeKind, string> = {
    [TypeKind.any]:     "any",
    [TypeKind.string]:  "string",
    [TypeKind.number]:  "number",
    [TypeKind.boolean]: "boolean",
    [TypeKind.nil]:     "nil",
    [TypeKind.array]:   "array",
    [TypeKind.tuple]:   "tuple",
    [TypeKind.struct]:  "struct",
    [TypeKind.union]:   "union",
}

const enum TypeFlags {
    optional = 1 << 1,
    const    = 1 << 2
}

if (TypeFlags.optional) {};

export interface TypeBase {
    kind: TypeKind,
    __debug_kind: string,
}

export function TypeBase<T extends TypeBase>(kind: T["kind"]) : T {
    const result = {kind} as T;

    if (debugTypeModule) {
        result.__debug_kind = TypeKindUiString[kind];
    }

    return result;
}

export type Type = cfAny | cfString | cfNumber | cfBoolean | cfNil | cfArray | cfTuple | cfStruct;

export interface cfAny extends TypeBase {
    kind: TypeKind.any;
}

export function cfAny() : cfAny {
    const v = TypeBase<cfAny>(TypeKind.any);
    return v;
}

export interface cfString extends TypeBase {
    kind: TypeKind.string,
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
    kind: TypeKind.number,
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
    kind: TypeKind.boolean,
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
    kind: TypeKind.nil
}

export function cfNil() : cfNil {
    const v = TypeBase<cfNil>(TypeKind.nil);
    return v;
}

export interface cfArray extends TypeBase {
    kind: TypeKind.array
    T: Type
}

export function cfArray(T: Type) : cfArray {
    const v = TypeBase<cfArray>(TypeKind.array);
    v.T = T;
    return v;
}

export interface cfTuple extends TypeBase {
    kind: TypeKind.tuple,
    T: Type[]
}

export function cfTuple(T: Type[]) : cfTuple {
    const v = TypeBase<cfTuple>(TypeKind.tuple);
    v.T = T;
    return v;
}

export interface cfStruct extends TypeBase {
    kind: TypeKind.struct,
    stringIndex: boolean;
    T: Map<string, Type>
}

export function cfStruct(T: Map<string, Type>, stringIndex = false) : cfStruct {
    const v = TypeBase<cfStruct>(TypeKind.struct);
    v.T = T;
    v.stringIndex = stringIndex;
    return v;
}
