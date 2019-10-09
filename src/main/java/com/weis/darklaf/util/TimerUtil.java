package com.weis.darklaf.util;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionListener;

/**
 * @author Jannis Weis
 */
public final class TimerUtil {
    @Contract("_, _, _ -> new")
    @NotNull
    public static Timer createNamedTimer(@NotNull final String name, final int delay,
                                         @NotNull final ActionListener listener) {
        return new Timer(delay, listener) {
            @Override
            public String toString() {
                return name;
            }
        };
    }

    @Contract("_, _ -> new")
    @NotNull
    public static Timer createNamedTimer(@NotNull final String name, final int delay) {
        return new Timer(delay, null) {
            @Override
            public String toString() {
                return name;
            }
        };
    }
}
