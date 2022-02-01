plugins {
    `java-library`
    `module-info-compile`
}

dependencies {
    api(projects.darklafThemeSpec)
    implementation(projects.darklafPlatformBase)
    implementation(projects.darklafPlatformDecorations)
    implementation(projects.darklafWindows)
    implementation(projects.darklafMacos)
}
