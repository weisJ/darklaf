package com.github.weisj.darklaf.ui.demo;

import java.util.Arrays;

public class EnumDemoSpec<T extends Enum<T>> extends DemoSpec<T> {

    public EnumDemoSpec(final String name, final Class<T> enumType, final SpecBinding<T> binding) {
        super(name, Arrays.asList(enumType.getEnumConstants()), binding);
    }
}
