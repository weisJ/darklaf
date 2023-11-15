plugins {
    `java-library`
}

dependencies {
    api(projects.darklafThemeSpec)
    implementation(projects.darklafPropertyLoader)
    implementation(projects.darklafUtils)

    compileOnly(projects.darklafAnnotations)
    compileOnly(toolLibs.errorprone.annotations)
    annotationProcessor(projects.darklafAnnotationsProcessor)

    compileOnly(toolLibs.autoservice.annotations)
    annotationProcessor(toolLibs.autoservice.processor)
}
