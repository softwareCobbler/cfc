import { exhaustiveCaseGuard } from "./utils";

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

    private compareInt(rhsMajor: number, rhsMinor: number, rhsPatch: number) : -1 | 0 | 1 {
        const compare = [
            intCompare(this.major, rhsMajor),
            intCompare(this.minor, rhsMinor),
            intCompare(this.patch, rhsPatch),
        ];

        for (const v of compare) {
            switch (v) {
                case -1: return -1;
                case 0: continue;
                case 1: return 1;
                default: exhaustiveCaseGuard(v);
            }
        }

        return 0;

        function intCompare(l: number, r: number) : -1 | 0 | 1 {
            return l < r ? -1 : l === r ? 0 : 1;
        }
    }
    
    public compare(rhsMajor: number, rhsMinor: number, rhsPatch: number) : -1 | 0 | 1 {
        return this.compareInt(rhsMajor, rhsMinor, rhsPatch);
    }

    eq(rhsMajor: number, rhsMinor?: number, rhsPatch?: number) {
        return this.compareInt(rhsMajor, rhsMinor as number, rhsPatch as number) === 0;
    }

    lt(rhsMajor: number, rhsMinor: number, rhsPatch: number) {
        return this.compareInt(rhsMajor, rhsMinor, rhsPatch) < 0;
    }

    lte(rhsMajor: number, rhsMinor: number, rhsPatch: number) {
        return this.compareInt(rhsMajor, rhsMinor, rhsPatch) <= 0;
    }

    gt(rhsMajor: number, rhsMinor: number, rhsPatch: number) {
        return this.compareInt(rhsMajor, rhsMinor, rhsPatch) > 0;
    }

    gte(rhsMajor: number, rhsMinor: number, rhsPatch: number) {
        return this.compareInt(rhsMajor, rhsMinor, rhsPatch) >= 0;
    }
}

export interface EngineVersion {
    engine: Engine,
    semver: Semver,
    uiString: string,
}

function LiteralRecord<T>() {
    return <U extends Record<string, T>>(v: U) : {[k in keyof U]: T} => v;
};

export const EngineVersions = LiteralRecord<EngineVersion>()({
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
} as const)

export const supports = {
    /**
     * something like:
     * " foo ".trim();
     * vs.
     * " foo "["trim"]();
     */
    bracketAccessIntoStringLiteral(ev: EngineVersion) {
        return ev.engine === Engine.Lucee;
    },
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