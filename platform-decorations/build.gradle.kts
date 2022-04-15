plugins {
    `java-library`
    `module-info-compile`
}

dependencies {
    api(projects.darklafPlatformBase)
    implementation(projects.darklafPropertyLoader)
    implementation(projects.darklafWindows)
    implementation(projects.darklafMacos)
}
