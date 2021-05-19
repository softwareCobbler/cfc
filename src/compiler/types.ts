import { copyFunctionParameterForTypePurposes, FunctionParameter, NodeBase, NodeId, NodeType, Terminal } from "./node";

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
    | cfTypeCall | cfCachedTypeConstructorInvocation | cfTypeConstructor | cfTypeConstructorParam;

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

export interface cfTypeCall extends TypeBase {
    typeKind: TypeKind.typeConstructorInvocation,
    left: Type,
    args: Type[]
}

export function cfTypeConstructorInvocation(left: Type, args: Type[]) : cfTypeCall {
    const v = TypeBase<cfTypeCall>(TypeKind.typeConstructorInvocation);
    v.left = left;
    v.args = args;
    return v;
}

export interface cfCachedTypeConstructorInvocation extends TypeBase {
    typeKind: TypeKind.cachedTypeConstructorInvocation,
    left: cfTypeConstructor,
    args: Type[],
}

function cfCachedTypeConstructorInvocation(left: cfTypeConstructor, args: Type[]) : cfCachedTypeConstructorInvocation {
    const v = TypeBase<cfCachedTypeConstructorInvocation>(TypeKind.cachedTypeConstructorInvocation);
    v.left = left;
    v.args = args;
    return v;
}

export interface cfTypeConstructor extends TypeBase {
    typeKind: TypeKind.typeConstructor,
    name: string,
    params: cfTypeConstructorParam[],
    body: Type,
}

export function cfTypeConstructor(params: cfTypeConstructorParam[], body: Type) : cfTypeConstructor {
    const v = TypeBase<cfTypeConstructor>(TypeKind.typeConstructor);
    v.params = params;
    v.body = body;
    return v;
}



export function KLUDGE_FOR_DEV_register_type_constructor_name(name: string, type: cfTypeConstructor) {
    typeCacheByName.set(name, type);
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

const typeCacheByNodeId = new Map<NodeId, Type>();
const typeCacheByName = new Map<string, Type>();

type NodeTrie = Map<NodeId, NodeTrie> & Map<null, Type | "PENDING"> ;

const typeCallCacheTrie : NodeTrie = new Map();

function KLUDGE_FOR_DEV_getTypeConstructorDefintionByName(name: string) {
    return typeCacheByName.get(name)! as cfTypeConstructor;
}

const enum TypeCacheResult { resolved, resolving, noCache };

function setCachedTypeCall(typeFunction: cfTypeConstructor, args: Type[], val: "PENDING" | Type) : void {
    function getChildTrieMapOrNull(thisLevel: NodeTrie, nodeId: NodeId) : NodeTrie | null {
        const result = thisLevel.get(nodeId);
        if (result && typeof result === "object") {
            return result;
        }
        else {
            return null;
        }
    }

    if (args.length === 0) {
        const bottom : NodeTrie = new Map();
        bottom.set(null, val);
        typeCallCacheTrie.set(typeFunction.nodeId, bottom);
        return;
    }

    let workingMap = getChildTrieMapOrNull(typeCallCacheTrie, typeFunction.nodeId);

    for (let i = 0; i < args.length; i++) { // is args ever 0 in a type call ?
        if (i === args.length - 1) {
            if (workingMap === null) {
                workingMap = new Map() as NodeTrie;
                typeCallCacheTrie.set(typeFunction.nodeId, workingMap);
            }
            const existingNextLevel = getChildTrieMapOrNull(workingMap, args[i].nodeId);
            if (existingNextLevel) {
                existingNextLevel.set(null, val);
            }
            else {
                const bottom = new Map([[null, val]]) as NodeTrie;
                workingMap.set(args[i].nodeId, bottom)
            }
        }
        else {
            if (workingMap === null) {
                workingMap = new Map() as NodeTrie;
                typeCallCacheTrie.set(typeFunction.nodeId, workingMap);
            }

            const existingNextLevel = getChildTrieMapOrNull(workingMap, args[i].nodeId);
            if (existingNextLevel) {
                workingMap = existingNextLevel;
            }
            else {
                const generatedNextLevel = new Map();
                workingMap.set(args[i].nodeId, generatedNextLevel);
                workingMap = generatedNextLevel;
            }
        }
    }
}

function getCachedTypeConstructorInvocation(typeFunction: cfTypeConstructor, args: Type[]) : {status: TypeCacheResult, value?: Type} {
    let trieDescender = typeCallCacheTrie.get(typeFunction.nodeId);
    for (let i = 0; i < args.length; i++) {
        if (!trieDescender) {
            return {status: TypeCacheResult.noCache};
        }
        trieDescender = trieDescender.get(args[i].nodeId);
    }

    if (!trieDescender) {
        return {status: TypeCacheResult.noCache}
    }

    const cachedResult = trieDescender.get(null);

    if (cachedResult === "PENDING") {
        return {status: TypeCacheResult.resolving};
    }
    else {
        return {status: TypeCacheResult.resolved, value: cachedResult}
    }
}

export function invokeTypeConstructor(typeFunction: cfTypeConstructor, args: Type[]) : Type {
    if (typeFunction.params.length !== args.length) {
        // hit this once by writing @type U<T>
        // when only @type T<U> was valid;
        // need some type checking of the type system
        throw "args.length !== typeFunction.params.length"
    }

    const cached = getCachedTypeConstructorInvocation(typeFunction, args);
    if (cached.status === TypeCacheResult.resolved) {
        return cached.value!;
    }

    const typeParamMap = new Map<string, Type>();
    for (let i = 0; i < typeFunction.params.length; i++) {
        typeParamMap.set(typeFunction.params[i].name, args[i]);
    }

    // say our current evaluation is for `T<U>`
    // set `T<U>` to "PENDING" so that we have something to check for to not recurse infinitely on something like `T<U> = {foo: T<U>}`
    setCachedTypeCall(typeFunction, args, "PENDING");
    const result = evaluateType(typeFunction.body, typeParamMap);

    typeCacheByNodeId.set(result.nodeId, result);
    setCachedTypeCall(typeFunction, args, result);
    return result;
}

/*function intersect(left: Type, right: Type) : Type[] {
    const types : Type[] = [];
    if (left.typeKind === TypeKind.intersection) {
        types.push(...left.typeList);
    }
    else {
        types.push(left);
    }
    if (right.typeKind === TypeKind.intersection) {
        types.push(...right.typeList);
    }
    else {
        types.push(right);
    }
    return types;
}*/

function evaluateIntersection(left: Type, right: Type, typeParamMap: Map<string, Type>) : Type {
    if (left.typeKind === TypeKind.struct && right.typeKind === TypeKind.struct) {
        let longest = left.members.size > right.members.size ? left.members : right.members;
        let shortest = longest === left.members ? right.members : left.members;

        const remainingLongestKeys = new Set([...longest.keys()]);
        const result = new Map<string, Type>();
        for (const key of shortest.keys()) {
            remainingLongestKeys.delete(key);
            const evaluatedShortest = evaluateType(shortest.get(key)!, typeParamMap);
            const evaluatedLongest = longest.has(key) ? evaluateType(longest.get(key)!, typeParamMap) : null;
            if (!evaluatedLongest) {
                result.set(key, evaluatedShortest);
                continue;
            }
            const intersect = evaluateIntersection(evaluatedShortest, evaluatedLongest, typeParamMap);
            if (intersect.typeKind === TypeKind.never) {
                return cfNever();
            }
            else {
                result.set(key, intersect);
            }
        }

        for (const key of remainingLongestKeys) {
            result.set(key, longest.get(key)!);
        }

        return cfStruct(result);
    }
    else {
        // only valid type operands to the "&" type operator are {}
        // which is not the "empty interface" but just a shorthand for "struct"
        return cfNever();
    }
}

export function evaluateType(type: Type | null, typeParamMap = new Map<string, Type>()) : Type {
    if (!type) return cfAny();
    
    switch (type.typeKind) {
        case TypeKind.intersection: {
            const left = evaluateType(type.left, typeParamMap);
            const right = evaluateType(type.right, typeParamMap);
            return evaluateIntersection(left, right, typeParamMap);
        }
        case TypeKind.union: {
            const left = evaluateType(type.left, typeParamMap);
            const right = evaluateType(type.right, typeParamMap);
            return cfUnion(left, right);
        }
        case TypeKind.struct: {
            const evaluatedStructContents = new Map<string, Type>();
            for (const key of type.members.keys()) {
                evaluatedStructContents.set(key, evaluateType(type.members.get(key)!, typeParamMap));
            }
            return cfStruct(evaluatedStructContents, type.stringIndex);
        }
        case TypeKind.functionSignature:
            // need a more lean version of functionParameter
            const params : FunctionParameter[] = type.params.map(param => copyFunctionParameterForTypePurposes(param));
            for (let i = 0; i < type.params.length; i++) {
                if (!params[i].type) {
                    throw "no type for parameter " + i; /// can we get here? parser / binder should convert this to any before we get here...
                }
                params[i].type = evaluateType(params[i].type!, typeParamMap);
            }
            const returns = evaluateType(type.returns, typeParamMap);
            return cfFunctionSignature(type.name, params, returns);
        case TypeKind.typeConstructorInvocation: {
            const typeConstructor = KLUDGE_FOR_DEV_getTypeConstructorDefintionByName((type.left as cfTypeId).name);
            const args : Type[] = [];
            for (const arg of type.args) {
                if (arg.typeKind === TypeKind.typeId) {
                    args.push(typeParamMap.get(arg.name)!);
                }
                else {
                    args.push(arg);
                }
            }

            const cachedTypeCall = getCachedTypeConstructorInvocation(typeConstructor, args);
            if (cachedTypeCall.status === TypeCacheResult.resolved) {
                return cachedTypeCall.value!;
            }
            else if (cachedTypeCall.status === TypeCacheResult.resolving) {
                return cfCachedTypeConstructorInvocation(typeConstructor, args);
            }
            else {
                return invokeTypeConstructor(typeConstructor, args);
            }
        }
        case TypeKind.cachedTypeConstructorInvocation: {
            const cachedTypeCall = getCachedTypeConstructorInvocation(type.left, type.args);
            if (cachedTypeCall.status === TypeCacheResult.resolved) {
                return cachedTypeCall.value!;
            }
            else if (cachedTypeCall.status === TypeCacheResult.resolving) {
                return type;
            }
            else {
                throw "expected resolved or resolving cache result but got none";
            }
        }
        case TypeKind.typeId: {
            const result = typeParamMap.get(type.name)!;
            if (result.typeKind === TypeKind.typeConstructor) {
                return invokeTypeConstructor(result, []);
            }
            else {
                return result;
            }
        }
        default:
            return type;
    }
}