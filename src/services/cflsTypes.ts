import { EngineVersion, EngineVersions } from "../compiler/engines";
import type { AbsPath, SafeOmit } from "../compiler/utils";

export type CflsRequest =
    | DiagnosticsRequest
    | CompletionsRequest
    | DefinitionLocationsRequest
    | InitRequest
    | ResetRequest
    | TrackRequest
    | UntrackRequest

export const enum CflsRequestType {
    diagnostics,
    completions,
    definitionLocations,
    init,
    reset,
    track,
    untrack
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
}

export interface InitRequest extends CflsRequestBase {
    type: CflsRequestType.init,
    initArgs: InitArgs,
}

export interface DiagnosticsRequest extends CflsRequestBase {
    type: CflsRequestType.diagnostics,
    fsPath: string,
    /**
     * null means "text was not changed"
     */
    freshText: string | null,
    /**
     * for emitting diagnostics of affected dependents, we'd like to see if we've seen a file in a particular batch and not go circular
     */
    batch: number
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

export interface TrackRequest extends CflsRequestBase {
    type: CflsRequestType.track,
    fsPath: string
}

export interface UntrackRequest extends CflsRequestBase {
    type: CflsRequestType.untrack,
    fsPath: string
}

export enum CflsResponseType {
    initialized,
    definitionLocations,
    diagnostics,
    completions,
    cancelled,
    track,
    untrack,
}

export type CflsResponse  =
    | InitializedResponse
    | DiagnosticsResponse
    | CompletionsResponse
    | DefinitionLocationsResponse
    | CancelledResponse
    | TrackResponse
    | UntrackResponse

interface CflsResponseBase {
    type: CflsResponseType,
    id: number,
}

interface InitializedResponse extends CflsResponseBase {
    type: CflsResponseType.initialized,
    // no other info, just "yes we are initialized"
}
interface DiagnosticsResponse extends CflsResponseBase {
    type: CflsResponseType.diagnostics,
    fsPath: AbsPath,
    diagnostics: unknown[],
    affectedDependents: AbsPath[],
    batch: number
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

interface TrackResponse extends CflsResponseBase {
    type: CflsResponseType.track
}

interface UntrackResponse extends CflsResponseBase {
    type: CflsResponseType.untrack
}

export interface CflsConfig {
	engineLibAbsPath: string | null
	x_types: boolean,
	engineVersion: EngineVersion,
	cfConfigProjectRelativePath: string | null,
}

export function CflsConfig() : CflsConfig {
    return {
        engineLibAbsPath: null,
        x_types: false,
        engineVersion: EngineVersions["lucee.5"],
        cfConfigProjectRelativePath: null,
    }
}

export function CflsInitArgs(): InitArgs["config"] {
    return {
        engineLibAbsPath: null,
        x_types: false,
        engineVersion: "lucee.5",
        cfConfigProjectRelativePath: null,
    }
}