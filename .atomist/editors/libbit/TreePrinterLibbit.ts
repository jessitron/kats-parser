import { File, Project } from "@atomist/rug/model/Core";
import { Editor, Parameter, Tags } from "@atomist/rug/operations/Decorators";
import { EditProject } from "@atomist/rug/operations/ProjectEditor";
import { Pattern } from "@atomist/rug/operations/RugOperation";

const EXISTING_PACKAGE="com.jessitron.kats";

/**
 * Run this to copy TreePrinter into your project
 */
@Editor("TreePrinterLibbit", "Run this to copy TreePrinter into your project")
@Tags("libbit")
export class TreePrinterLibbit implements EditProject {

    @Parameter({ pattern: Pattern.any,
    description: "Package to put the libbit class in"})
    public destinationPackage: string;

    public edit(project: Project) {

        const destinationPackage = this.destinationPackage;

        const sourceFiles = [ "src/main/scala/com/jessitron/kats/TreePrinter.scala" ];
        const testFiles = [ "src/test/scala/com/jessitron/kats/TreePrinterTest.scala" ];

        const allFiles = sourceFiles.concat(testFiles);

        const newPath = (formerPath: string) =>
            replacePortion(EXISTING_PACKAGE, destinationPackage, formerPath);

        const alreadyExisting = allFiles.filter((f) => project.fileExists(newPath(f)));
        if (alreadyExisting.length > 0) {
            console.log(`File ${alreadyExisting.join(" and ")} already exists. Exiting`);
            return;
        }

        allFiles.forEach((f) => {
            const destinationPath = newPath(f);
            project.copyEditorBackingFileOrFailToDestination(f,
                destinationPath);

            const file = project.findFile(destinationPath);
            file.setContent(file.content.replace(EXISTING_PACKAGE, destinationPackage))
        });

        // TODO: some atomist config operation to make a link
    }


}

function replacePortion(currentPackage: string, destPackage: string, path: string) {
    return path.replace(packageToDir(currentPackage), packageToDir(destPackage));
}

function packageToDir(pp: string) {
    return pp.replace(/\./g, "/");
}

export const sampleLibbit = new TreePrinterLibbit();
