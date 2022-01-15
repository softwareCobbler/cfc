import { EngineVersion, EngineVersions } from "../compiler/engines";
import type { AbsPath, SafeOmit } from "../compiler/utils";

export type CflsRequest =
    | DiagnosticsRequest
    | CompletionsRequest
    | DefinitionLocationsRequest
    | InitRequest
    | ResetRequest

export const enum CflsRequestType {
    diagnostics,
    completions,
    definitionLocations,
    init,
    reset
}

interface CflsRequestBase {
    type: CflsRequestType,
    id: number,
}

// EngineVersion isn't serializeable, we need to transport just the engine name
export type SerializableCflsConfig = SafeOmit<CflsConfig, "engineVersion"> & {engineVersion: keyof typeof EngineVersions};
export interface InitArgs {
    config: SerializableCflsConfig,
    workspaceRoots: AbsPath[],
    cancellationTokenId: string,
    clientAdaptersFilePath: AbsPath,
}

export interface InitRequest extends CflsRequestBase {
    type: CflsRequestType.init,
    initArgs: InitArgs,
}

export interface DiagnosticsRequest extends CflsRequestBase {
    type: CflsRequestType.diagnostics,
    fsPath: string,
    freshText: string
}

export interface CompletionsRequest extends CflsRequestBase {
    type: CflsRequestType.completions,
    fsPath: string,
    targetIndex: number,
    triggerCharacter: string | null
}

export interface DefinitionLocationsRequest extends CflsRequestBase {
    type: CflsRequestType.definitionLocations,
    fsPath: string,
    targetIndex: number
}
export interface ResetRequest extends CflsRequestBase {
    type: CflsRequestType.reset
    config: SerializableCflsConfig
}

export const enum CflsResponseType {
    definitionLocations,
    diagnostics,
    completions,
    cancelled
}

export type CflsResponse  =
    | DiagnosticsResponse
    | CompletionsResponse
    | DefinitionLocationsResponse
    | CancelledResponse

interface CflsResponseBase {
    type: CflsResponseType,
    id: number,
}

interface DiagnosticsResponse extends CflsResponseBase {
    type: CflsResponseType.diagnostics,
    fsPath: AbsPath,
    diagnostics: unknown[]
}

interface CompletionsResponse extends CflsResponseBase {
    type: CflsResponseType.completions,
    fsPath: AbsPath,
    completionItems: unknown[]
}

interface DefinitionLocationsResponse extends CflsResponseBase {
    type: CflsResponseType.definitionLocations,
    locations: unknown[]
}
interface CancelledResponse extends CflsResponseBase {
    type: CflsResponseType.cancelled
}

export interface CflsConfig {
	engineLibAbsPath: string | null
	x_parseTypes: boolean,
	engineVersion: EngineVersion,
	wireboxConfigFile: string | null,
	wireboxResolution: boolean,
	x_checkReturnTypes: boolean,
	x_genericFunctionInference: boolean,
}

export function CflsConfig() : CflsConfig {
    return {
        engineLibAbsPath: null,
        x_parseTypes: false,
        engineVersion: EngineVersions["lucee.5"],
        wireboxConfigFile: null,
        wireboxResolution: false,
        x_checkReturnTypes: false,
        x_genericFunctionInference: false,
    }
}

export function CflsInitArgs(): InitArgs["config"] {
    return {
        engineLibAbsPath: null,
        x_parseTypes: false,
        engineVersion: "lucee.5",
        wireboxConfigFile: null,
        wireboxResolution: false,
        x_checkReturnTypes: false,
        x_genericFunctionInference: false,
    }
}