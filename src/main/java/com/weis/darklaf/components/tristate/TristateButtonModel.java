package com.weis.darklaf.components.tristate;

import javax.swing.*;
import java.awt.event.ItemEvent;

public class TristateButtonModel extends JToggleButton.ToggleButtonModel {
    private TristateState state = TristateState.DESELECTED;

    public TristateButtonModel(final TristateState state) {
        setState(state);
    }

    public TristateButtonModel() {
        this(TristateState.DESELECTED);
    }

    public void setIndeterminate() {
        setState(TristateState.INDETERMINATE);
    }

    public boolean isIndeterminate() {
        return state == TristateState.INDETERMINATE;
    }

    // Overrides of superclass methods
    public void setEnabled(final boolean enabled) {
        super.setEnabled(enabled);
        // Restore state display
        displayState();
    }

    public void setSelected(final boolean selected) {
        setState(selected ? TristateState.SELECTED : TristateState.DESELECTED);
    }

    @Override
    public boolean isSelected() {
        return state == TristateState.SELECTED;
    }

    // Empty overrides of superclass methods
    public void setArmed(final boolean b) {
    }

    public void setPressed(final boolean b) {
    }

    protected void iterateState() {
        setState(state.next());
    }

    public void setState(final TristateState state) {
        //Set internal state
        this.state = state;
        displayState();
        if (state == TristateState.INDETERMINATE && isEnabled()) {
            // force the events to fire

            // Send ChangeEvent
            fireStateChanged();

            // Send ItemEvent
            int indeterminate = 3;
            //noinspection MagicConstant
            fireItemStateChanged(new ItemEvent(this, ItemEvent.ITEM_STATE_CHANGED, this, indeterminate));
        }
    }

    protected void displayState() {
        super.setSelected(state != TristateState.DESELECTED);
        super.setArmed(state == TristateState.INDETERMINATE);
        super.setPressed(state == TristateState.INDETERMINATE);
    }

    public TristateState getState() {
        return state;
    }
}
