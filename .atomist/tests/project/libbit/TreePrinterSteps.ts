import { Project } from "@atomist/rug/model/Project";
import {
    Given, ProjectScenarioWorld, Then, When,
} from "@atomist/rug/test/project/Core";

const sourceFiles = [ "src/main/scala/i/love/libbits/TreePrinter.scala" ];
const testFiles = [ "src/test/scala/i/love/libbits/TreePrinterTest.scala" ];

When("the TreePrinterLibbit is run", (p: Project, world) => {
    const w = world as ProjectScenarioWorld;
    const editor = w.editor("TreePrinterLibbit");
    w.editWith(editor, { destinationPackage: "i.love.libbits"});
});

Then("the new TreePrinter source file exists", (p: Project, world) => {
    return sourceFiles.every((f) => p.fileExists(f));
});

Then("the new TreePrinter test files exist", (p: Project, world) => {
    return testFiles.every((f) => p.fileExists(f));
});

Given("the new TreePrinter source file already exists", (p: Project) => {
    p.addFile(sourceFiles[0], "stuff");
});
