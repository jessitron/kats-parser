import {Project} from "@atomist/rug/model/Core";
import {Editor, Parameter, Tags} from "@atomist/rug/operations/Decorators";
import {EditProject} from "@atomist/rug/operations/ProjectEditor";
import {incrementVersion} from "./libbits/IncrementVersion";

/**
 * Sample TypeScript editor used by AddBumpVersion.
 */
@Editor("BumpVersion", "increment the version of this project")
@Tags("documentation")
export class BumpVersion implements EditProject {

    @Parameter({
        displayName: "version component",
        description: "which component of the version to increment, major|minor|patch",
        pattern: "^(major|minor|patch)$",
        validInput: "one of: major, minor, patch",
        minLength: 5,
        maxLength: 5,
        required: false,
    })
    public component: string = "patch";

    public edit(project: Project) {

        // there are more rigorous ways to update the pom ... where's the editor for this?
        const pom = project.findFile("pom.xml");
        const currentVersion = pom.content.match(/version>(.*)<\/version>/)[1];
        const newVersion = incrementVersion(currentVersion, this.component as any);

        pom.setContent(pom.content.replace(/version>(.*)<\/version>/,
            `version>${newVersion}</version`)); // only the first one


        // Now the custom bit
        const certainFile = project.findFile("src/main/scala/com/jessitron/kats/ElmParser.scala");
        const newContent = certainFile.content.replace(/VERSION = ".*"/, `VERSION = "${newVersion}"`);
        certainFile.setContent(newContent);
    }
}

export const bumpVersion = new BumpVersion();
