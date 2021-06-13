/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
package com.github.weisj.darklaf.annotations.processor;

import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.ElementFilter;
import javax.tools.JavaFileObject;

import com.github.weisj.darklaf.annotations.SynthesiseLaf;

/**
 * @deprecated Auto-Generated class. Use LafManager.installTheme(Theme) instead.
 */
@SupportedAnnotationTypes("com.github.weisj.darklaf.annotations.SynthesiseLaf")
public class SynthesiseLafProcessor extends AbstractProcessor {

    private static final String IDENT = "    ";

    @Override
    public boolean process(final Set<? extends TypeElement> annotations, final RoundEnvironment roundEnv) {
        Collection<? extends Element> annotatedElements = roundEnv.getElementsAnnotatedWith(SynthesiseLaf.class);
        List<TypeElement> types = ElementFilter.typesIn(annotatedElements);
        String packageName = "com.github.weisj.darklaf.theme.laf";
        String baseClassName = "SynthesisedThemedLaf";

        for (TypeElement typeElement : types) {
            String themeName = typeElement.getSimpleName().toString();
            String themePath = typeElement.getQualifiedName().toString();
            String synthesisedClassName = themeName + "DarklafLookAndFeel";
            String synthesisedName = packageName + "." + synthesisedClassName;

            StringBuilder builder = new StringBuilder();
            builder.append("package ").append(packageName).append(";\n\n");

            builder.append("import ").append(themePath).append(";\n\n");
            builder.append("/**\n");
            builder.append(" * @deprecated Auto-Generated class. Use LafManager.installTheme(Theme) instead.\n");
            builder.append(" */\n");
            builder.append("@Deprecated\n");
            builder.append("public class ").append(synthesisedClassName).append(" extends ").append(baseClassName);
            builder.append(" {\n\n");
            builder.append(IDENT).append("public ").append(synthesisedClassName).append("() {\n");
            builder.append(IDENT).append(IDENT).append("super(new ").append(themeName).append("());\n");
            builder.append(IDENT).append("}\n").append("}");

            try {
                JavaFileObject javaFileObject = processingEnv.getFiler().createSourceFile(synthesisedName, typeElement);
                Writer writer = javaFileObject.openWriter();
                writer.write(builder.toString());
                writer.close();
            } catch (final IOException e) {
                e.printStackTrace();
            }
        }
        return false;
    }

    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }
}
