package com.weis.darklaf.components;

import java.beans.PropertyChangeEvent;

public class TabPropertyChangeEvent extends PropertyChangeEvent {

    private final int index;

    public TabPropertyChangeEvent(final Object source, final String propertyName,
                                  final Object oldValue, final Object newValue, final int index) {
        super(source, propertyName, oldValue, newValue);
        this.index = index;
    }

    public int getIndex() {
        return index;
    }
}
