package com.weis.darklaf.decorators;

import org.jetbrains.annotations.Contract;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.function.Consumer;

public class MouseResponder implements MouseListener {

    private final Consumer<MouseEvent> consumer;

    @Contract(pure = true)
    public MouseResponder(final Consumer<MouseEvent> consumer) {
        this.consumer = consumer;
    }

    @Override
    public void mouseClicked(final MouseEvent e) {
        if (consumer != null) {
            consumer.accept(e);
        }
    }

    @Override
    public void mousePressed(final MouseEvent e) {
        if (consumer != null) {
            consumer.accept(e);
        }
    }

    @Override
    public void mouseReleased(final MouseEvent e) {
        if (consumer != null) {
            consumer.accept(e);
        }
    }

    @Override
    public void mouseEntered(final MouseEvent e) {
        if (consumer != null) {
            consumer.accept(e);
        }
    }

    @Override
    public void mouseExited(final MouseEvent e) {
        if (consumer != null) {
            consumer.accept(e);
        }
    }
}
