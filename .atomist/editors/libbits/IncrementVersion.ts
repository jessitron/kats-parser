export function incrementVersion(version: string, component: "major" | "minor" | "patch"): string {
    const versionRegex = /^(\d+)\.(\d+)\.(\d+)([-.].*)?$/;
    const versionMatch = versionRegex.exec(version);
    if (versionMatch === null || versionMatch.length < 4) {
        throw new Error(`version does not appear to be valid: ${version}`);
    }

    let major = parseInt(versionMatch[1], 10);
    let minor = parseInt(versionMatch[2], 10);
    let patch = parseInt(versionMatch[3], 10);
    const rest = (versionMatch[4] != null) ? versionMatch[4] : "";

    if (component === "major") {
        major = major + 1;
        minor = 0;
        patch = 0;
    } else if (component === "minor") {
        minor = minor + 1;
        patch = 0;
    } else if (component === "patch") {
        patch = patch + 1;
    }
    return `${major}.${minor}.${patch}${rest}`;
}
