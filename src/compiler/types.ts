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
    typeFunctionDefinition,
    typeCall,
    typeFunctionParam,
    typeId,
}

const TypeKindUiString : Record<TypeKind, string> = {
    [TypeKind.any]:                    "any",
    [TypeKind.void]:                   "void",
    [TypeKind.string]:                 "string",
    [TypeKind.number]:                 "number",
    [TypeKind.boolean]:                "boolean",
    [TypeKind.nil]:                    "nil",
    [TypeKind.array]:                  "array",
    [TypeKind.tuple]:                  "tuple",
    [TypeKind.struct]:                 "struct",
    [TypeKind.union]:                  "union",
    [TypeKind.intersection]:           "intersection",
    [TypeKind.functionSignature]:      "function-signature",  // (name: type, ...) => type
    [TypeKind.typeCall]:               "type-call",           // type<type-list, ...>
    [TypeKind.typeFunctionDefinition]: "type-function",       // type<type-list, ...> => type
    [TypeKind.typeFunctionParam]:      "type-function-param", // type param in a type function
    [TypeKind.typeId]:                 "type-id",             // name of non-builtin type, e.g, "T"
    [TypeKind.never]:                  "never",
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
    | cfTypeId | cfNever
    | cfTuple | cfStruct | cfFunctionSignature | cfTypeCall | cfTypeFunctionDefinition | cfTypeFunctionParam;

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
    typeKind: TypeKind.typeCall,
    left: Type,
    args: Type[]
}

export function cfTypeCall(left: Type, args: Type[]) : cfTypeCall {
    const v = TypeBase<cfTypeCall>(TypeKind.typeCall);
    v.left = left;
    v.args = args;
    return v;
}

export interface cfTypeFunctionDefinition extends TypeBase {
    typeKind: TypeKind.typeFunctionDefinition,
    name: string,
    params: cfTypeFunctionParam[],
    body: Type,
}

export function cfTypeFunctionDefinition(params: cfTypeFunctionParam[], body: Type) : cfTypeFunctionDefinition {
    const v = TypeBase<cfTypeFunctionDefinition>(TypeKind.typeFunctionDefinition);
    v.params = params;
    v.body = body;
    return v;
}

export function KLUDGE_FOR_DEV_register_type_function_name(name: string, type: cfTypeFunctionDefinition) {
    typeCacheByName.set(name, type);
}

export interface cfTypeFunctionParam extends TypeBase {
    typeKind: TypeKind.typeFunctionParam,
    name: string,
    extendsToken: Terminal | null,
    extendsType: Type | null,
    comma: Terminal | null
}

export function cfTypeFunctionParam(name: string, extendsToken: Terminal | null, extendsType: Type | null, comma: Terminal | null) {
    const v = TypeBase<cfTypeFunctionParam>(TypeKind.typeFunctionParam);
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
    v.typeList = [];
    return v;
}

export interface cfUnion extends TypeBase {
    typeKind: TypeKind.union,
    left: Type,
    right: Type,
}

export interface cfNever extends TypeBase {
    typeKind: TypeKind.never,
}

export function cfNever() : cfNever {
    const v = TypeBase<cfNever>(TypeKind.never);
    return v;
}

const typeCacheByNodeId = new Map<NodeId, Type>();
const typeCacheByName = new Map<string, Type>();

type NodeTrie = Map<null, Type | "PENDING"> & Map<NodeId, NodeTrie>;

const typeCallCacheTrie : NodeTrie = new Map();

function getTypeFunctionDefintionByName(name: string) {
    return typeCacheByName.get(name)! as cfTypeFunctionDefinition;
}

const enum TypeCacheResult { resolved, resolving, noCache };

function setCachedTypeCall(typeFunction: cfTypeFunctionDefinition, args: Type[], val: "PENDING" | Type) : void {
    function getChildTrieMapOrNull(thisLevel: NodeTrie, nodeId: NodeId) : NodeTrie | null {
        const result = thisLevel.get(nodeId);
        if (result && typeof result === "object") {
            return result;
        }
        else {
            return null;
        }
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

function getCachedTypeCall(typeFunction: cfTypeFunctionDefinition, args: Type[]) : {status: TypeCacheResult, value?: Type} {
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

export function evaluateTypeCall(typeFunction: cfTypeFunctionDefinition, args: Type[]) : Type {
    if (typeFunction.params.length !== args.length) {
        throw "args.length !== typeFunction.params.length"
    }

    const cached = getCachedTypeCall(typeFunction, args);
    if (cached.status === TypeCacheResult.resolved) {
        return cached.value!;
    }

    const typeParamMap = new Map<string, Type>();
    for (let i = 0; i < typeFunction.params.length; i++) {
        typeParamMap.set(typeFunction.params[i].name, args[i]);
    }

    setCachedTypeCall(typeFunction, args, "PENDING");
    const result = evaluateType(typeFunction.body, typeParamMap);

    typeCacheByNodeId.set(result.nodeId, result);
    setCachedTypeCall(typeFunction, args, result);
    return result;
}

function intersect(left: Type, right: Type) : Type[] {
    const types : Type[] = [];
    if (left.typeKind === TypeKind.intersection) {
        types.push(left.left, left.right);
    }
    else {
        types.push(left);
    }
    if (right.typeKind === TypeKind.intersection) {
        types.push(right.left, right.right);
    }
    else {
        types.push(right);
    }
    return types;
}

export function evaluateType(type: Type, typeParamMap = new Map<string, Type>()) : Type {
    switch (type.typeKind) {
        case TypeKind.intersection: {
            const left = evaluateType(type.left, typeParamMap);
            const right = evaluateType(type.right, typeParamMap)
            const result = cfIntersection(cfNil(), cfNil());
            result.typeList = intersect(left, right);
            return result;
        }
        case TypeKind.struct: {
            const evaluatedStructContents = new Map<string, Type>();
            for (const key of type.T.keys()) {
                evaluatedStructContents.set(key, evaluateType(type.T.get(key)!, typeParamMap));
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
        case TypeKind.typeCall:
            const typeFunction = getTypeFunctionDefintionByName((type.left as cfTypeId).name);
            const args : Type[] = [];
            for (const arg of type.args) {
                if (arg.typeKind === TypeKind.typeId) {
                    args.push(typeParamMap.get(arg.name)!);
                }
                else {
                    args.push(arg);
                }
            }

            const cachedTypeCall = getCachedTypeCall(typeFunction, args);
            if (cachedTypeCall.status === TypeCacheResult.resolved) {
                return cachedTypeCall.value!;
            }
            else if (cachedTypeCall.status === TypeCacheResult.resolving) {
                return type;
            }
            else {
                evaluateTypeCall(typeFunction, args);
            }
        case TypeKind.number:
        case TypeKind.string:
        case TypeKind.boolean:
        case TypeKind.any:
        case TypeKind.void:
            return type;
        case TypeKind.typeId:
            return typeParamMap.get(type.name)!
        default:
            throw "not yet implemented";
    }
}