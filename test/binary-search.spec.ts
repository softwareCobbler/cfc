import * as assert from "assert";
import { binarySearch } from "../out/compiler";

function find(ns: number[], target: number) {
    const result = binarySearch(ns, (v) => v < target ? -1 : v === target ? 0 : 1);
    return result;
}

describe("Binary search works", () => {
    it("should find the thing", () => {
        const things = [1,3,5];
        assert.strictEqual(find(things, 1), 0);
        assert.strictEqual(find(things, 3), 1);
        assert.strictEqual(find(things, 5), 2);
        assert.strictEqual(find(things, 0), ~0);
        assert.strictEqual(find(things, 7), ~(things.length-1));
    });
    it("should find the thing", () => {
        const things = [1,3,5,7];
        assert.strictEqual(find(things, 1), 0);
        assert.strictEqual(find(things, 3), 1);
        assert.strictEqual(find(things, 5), 2);
        assert.strictEqual(find(things, 7), 3);
        assert.strictEqual(find(things, 0), ~0);
        assert.strictEqual(find(things, 9), ~(things.length-1));
    });
    it("should find the thing", () => {
        const things = [1];
        assert.strictEqual(find(things, 1), 0);
        assert.strictEqual(find(things, 0), ~0);
        assert.strictEqual(find(things, 7), ~(things.length-1));
    });
    it("should find the thing", () => {
        const things : number[] = [];
        assert.strictEqual(find(things, 1), ~0);
        assert.strictEqual(find(things, 7), ~0);
    });
})
