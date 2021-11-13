import { EngineVersion, EngineVersions } from "../compiler/engines";

type AbsPath = string;

export const enum CflsResponseType {
    diagnostic
}

export type CflsResponse  =
    | DiagnosticResponse

interface CflsResponseBase {
    type: CflsResponseType,
}

interface DiagnosticResponse extends CflsResponseBase {
    type: CflsResponseType.diagnostic,
    fsPath: AbsPath,
    diagnostics: unknown[]
}

export type CflsRequest =
    | DiagnosticRequest
    | InitRequest
    | ResetRequest

export const enum CflsRequestType {
    diagnostic,
    init,
    reset
}

interface CflsRequestBase {
    type: CflsRequestType,
}

export interface InitRequest extends CflsRequestBase {
    type: CflsRequestType.init,
    initArgs: {
        config: CflsConfig,
        workspaceRoots: AbsPath[],
        clientAdaptersFilePath: AbsPath,
    }
}

export interface DiagnosticRequest extends CflsRequestBase {
    type: CflsRequestType.diagnostic,
    fsPath: string,
    freshText: string
}

export interface ResetRequest extends CflsRequestBase {
    type: CflsRequestType.reset
    config: CflsConfig
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