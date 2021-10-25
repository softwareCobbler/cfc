export const enum Engine { Adobe = 1, Lucee = 2 };

export class Semver {
    major: number;
    minor: number;
    patch: number;
    constructor(major: number, minor: number, patch: number) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;
    }

    raw() {
        return [this.major, this.minor, this.patch];
    }

    eq(rhsMajor: number, rhsMinor: number, rhsPatch: number) {
        return this.major == rhsMajor && this.minor === rhsMinor && this.patch === rhsPatch;
    }

    lt(rhsMajor: number, rhsMinor: number, rhsPatch: number) {
        return this.major < rhsMajor || this.minor < rhsMinor || this.patch < rhsPatch;
    }

    lte(rhsMajor: number, rhsMinor: number, rhsPatch: number) {
        return this.eq(rhsMajor, rhsMinor, rhsPatch) || this.major < rhsMajor || this.minor < rhsMinor || this.patch < rhsPatch;
    }

    gt(rhsMajor: number, rhsMinor: number, rhsPatch: number) {
        return this.major > rhsMajor || this.minor > rhsMinor || this.patch > rhsPatch;
    }

    gte(rhsMajor: number, rhsMinor: number, rhsPatch: number) {
        return this.eq(rhsMajor, rhsMinor, rhsPatch) || this.major > rhsMajor || this.minor > rhsMinor || this.patch > rhsPatch;
    }
}

export interface EngineVersion {
    engine: Engine,
    semver: Semver,
    uiString: string,
}

export const EngineVersions = {
    "acf.2018": {
            engine: Engine.Adobe,
            semver: new Semver(2018, 0, 0),
            uiString: "Adobe/2018"
    },
    "acf.2021": {
        engine: Engine.Adobe,
        semver: new Semver(2021, 0, 0),
        uiString: "Adobe/2021",
    },
    "lucee.5": {
        engine: Engine.Lucee,
        semver: new Semver(5, 0, 0),
        uiString: "Lucee/5",
    }
} as const;

export const supports = {
    trailingStructLiteralComma(ev: EngineVersion) {
        return ev.engine === Engine.Lucee;
    },
    trailingArrayLiteralComma(ev: EngineVersion) {
        return ev.engine === Engine.Lucee;
    },
    structLiteralSpread(ev: EngineVersion) {
        return ev.engine === Engine.Adobe && ev.semver.gte(2021, 0, 0);
    },
    structLiteralShorthand(ev: EngineVersion) {
        return ev.engine === Engine.Adobe && ev.semver.gte(2021, 0, 0);
    },
    noParenSingleArgArrowFunction(ev: EngineVersion) {
        return ev.engine === Engine.Adobe;
    }
} as const;