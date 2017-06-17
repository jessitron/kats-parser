/*
 * Copyright © 2017 Atomist, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import "mocha";
import assert = require("power-assert");

import { incrementVersion } from "../editors/libbits/IncrementVersion";

describe("incrementVersion", () => {

    it("should increment the major version", () => {
        const version = "1.9.84";
        const newVersion = incrementVersion(version, "major");
        assert(newVersion === "2.0.0");
    });

    it("should increment the minor version", () => {
        const version = "1.9.84";
        const newVersion = incrementVersion(version, "minor");
        assert(newVersion === "1.10.0");
    });

    it("should increment the patch level", () => {
        const version = "1.9.84";
        const newVersion = incrementVersion(version, "patch");
        assert(newVersion === "1.9.85");
    });

    it("should fail if version is not valid", () => {
        const version = "1984";
        assert.throws(() => incrementVersion(version, "major"));
    });

    it("should increment snapshot version", () => {
        const version = "1.9.84-SNAPSHOT";
        const newVersion = incrementVersion(version, "major");
        assert(newVersion === "2.0.0-SNAPSHOT");
    });

    it("should fail if version has extra stuff at end", () => {
        const version = "1.9.84foo";
        assert.throws(() => incrementVersion(version, "minor"));
    });

});
