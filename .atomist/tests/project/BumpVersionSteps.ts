import { Project } from "@atomist/rug/model/Project";
import {
    Given, ProjectScenarioWorld, Then, When,
} from "@atomist/rug/test/project/Core";

const POM = `<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

<groupId>jessitron</groupId>
<artifactId>kats-parser</artifactId>
<version>0.2.1</version>

<dependencies>
        <dependency>
            <groupId>org.scala-lang</groupId>
            <artifactId>scala-library</artifactId>
            <version>0.2.1</version>
        </dependency>`;

const POM_AFTER = `<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

<groupId>jessitron</groupId>
<artifactId>kats-parser</artifactId>
<version>0.3.0</version>

<dependencies>
        <dependency>
            <groupId>org.scala-lang</groupId>
            <artifactId>scala-library</artifactId>
            <version>0.2.1</version>
        </dependency>`;

const CERTAIN_INPUT_FILEPATH = "src/main/scala/com/jessitron/kats/ElmParser.scala";

const CERTAIN_FILE_CONTENT_BEFORE = `object ElmParser extends RegexParsers {

  val VERSION = "0.2.1"

}
`;

const CERTAIN_FILE_CONTENT_AFTER = `object ElmParser extends RegexParsers {

  val VERSION = "0.3.0"

}
`;

Given("a project with the version in ElmParser.scala", (p: Project, world) => {
    p.addFile(CERTAIN_INPUT_FILEPATH, CERTAIN_FILE_CONTENT_BEFORE);
    p.addFile("pom.xml", POM);
});

When("the BumpVersion is run", (p: Project, world) => {
    const w = world as ProjectScenarioWorld;
    const editor = w.editor("BumpVersion");
    w.editWith(editor, { component: "minor" });
});

Then("that version number looks different", (p: Project, world) => {
    const w = world as ProjectScenarioWorld;
    const after = p.findFile(CERTAIN_INPUT_FILEPATH).content;
    const passing = (after === CERTAIN_FILE_CONTENT_AFTER);
    if (!passing) {
        console.log(`FAILURE: ${CERTAIN_INPUT_FILEPATH} --->\n${after}\n<---`);
    }
    return passing;
});

Then("the version number is updated in the pom", (p: Project, world) => {
    const w = world as ProjectScenarioWorld;
    const after = p.findFile(CERTAIN_INPUT_FILEPATH).content;
    const passing = (after === CERTAIN_FILE_CONTENT_AFTER);
    if (!passing) {
        console.log(`FAILURE: ${CERTAIN_INPUT_FILEPATH} --->\n${after}\n<---`);
    }
    return passing;
});
