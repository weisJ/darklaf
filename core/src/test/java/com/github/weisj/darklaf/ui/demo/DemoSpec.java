package com.github.weisj.darklaf.ui.demo;

import javax.swing.JComponent;
import java.util.List;

public class DemoSpec<T> {
    private final String name;
    private final List<T> values;
    private final SpecBinding<T> binding;

    public DemoSpec(final String name, final List<T> values, final SpecBinding<T> binding) {
        this.name = name;
        this.values = values;
        this.binding = binding;
    }

    public List<T> getPossibleValues() {
        return values;
    }

    public T get(final JComponent component) {
        return binding.get(component);
    }

    public void set(final JComponent component, final T value) {
        binding.set(component, value);
    }

    public String getName() {
        return name;
    }
}
