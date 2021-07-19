package com.github.weisj.darklaf.ui.demo;

import javax.swing.JComponent;

@SuppressWarnings("unchecked")
public class ClientPropertySpecBinding<T> implements SpecBinding<T> {

    private final String key;

    public ClientPropertySpecBinding(final String key) {
        this.key = key;
    }

    @Override
    public T get(final JComponent component) {
        return (T) component.getClientProperty(key);
    }

    @Override
    public void set(final JComponent component, final T value) {
        component.putClientProperty(key, value);
    }
}
