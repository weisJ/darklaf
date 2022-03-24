plugins {
    `java-library`
    `module-info-compile`
}

dependencies {
    implementation(projects.darklafPropertyLoader)
    implementation(projects.darklafPlatformBase)
    implementation(projects.darklafWindows)
    implementation(projects.darklafMacos)
}
