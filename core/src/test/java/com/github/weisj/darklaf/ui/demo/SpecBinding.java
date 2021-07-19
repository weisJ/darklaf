package com.github.weisj.darklaf.ui.demo;

import javax.swing.JComponent;

public interface SpecBinding<T> {

    T get(final JComponent component);

    void set(final JComponent component, final T value);
}
