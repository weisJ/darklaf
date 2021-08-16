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
package com.github.weisj.darklaf.util.log;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Locale;
import java.util.Set;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import com.github.weisj.darklaf.util.StringUtil;

/**
 * The default log formatter for darklaf loggers.
 *
 * @author Jannis Weis
 */
public class LogFormatter extends Formatter {
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_BLACK = "\u001B[30m";
    public static final String ANSI_RED = "\u001B[31m";
    public static final String ANSI_GREEN = "\u001B[32m";
    public static final String ANSI_YELLOW = "\u001B[33m";
    public static final String ANSI_BLUE = "\u001B[34m";
    public static final String ANSI_PURPLE = "\u001B[35m";
    public static final String ANSI_CYAN = "\u001B[36m";
    public static final String ANSI_WHITE = "\u001B[37m";
    public static final String ANSI_BOLD_ON = "\u001B[01m";
    public static final String ANSI_BOLD_OFF = "\u001B[2m";

    private final DateTimeFormatter dateTimeFormatter =
            DateTimeFormatter.ofLocalizedDateTime(FormatStyle.SHORT)
                    .withLocale(Locale.UK)
                    .withZone(ZoneId.systemDefault());

    @Override
    public String format(final LogRecord record) {
        StringBuilder builder = new StringBuilder();
        builder.append(ANSI_BLUE);

        String time = calculateDateString(record.getMillis());
        builder.append("[");
        builder.append(time);
        builder.append("]");

        builder.append(ANSI_YELLOW);

        builder.append(" [");
        builder.append(record.getLevel().getName());
        builder.append("]");

        builder.append(ANSI_RESET);
        builder.append(getMessageColor(record));
        builder.append(" ");
        builder.append(record.getMessage());

        builder.append(ANSI_RESET);
        builder.append(ANSI_BOLD_ON);
        builder.append(" [at ");
        builder.append(record.getSourceClassName());
        builder.append("]");
        builder.append(ANSI_BOLD_OFF);

        Object[] params = record.getParameters();

        int spaceLength = time.length() + 3 + record.getLevel().getName().length() + 3;
        String space = StringUtil.repeat(" ", spaceLength);
        if (params != null) {
            builder.append("\n");
            builder.append(StringUtil.repeat(" ", spaceLength - 10));
            builder.append(ANSI_YELLOW);
            builder.append("[Details] ");
            builder.append(getMessageColor(record));
            for (int i = 0; i < params.length; i++) {
                builder.append(params[i]);
                if (i < params.length - 1) {
                    builder.append(",\n");
                    builder.append(space);
                }
            }
        }

        builder.append(ANSI_RESET);
        builder.append("\n");
        if (record.getThrown() != null) {
            builder.append(getMessageColor(record));
            appendExceptionMessage(builder, record.getThrown());
        }

        return builder.toString();
    }

    private void appendExceptionMessage(final StringBuilder builder, final Throwable throwable) {
        builder.append(throwable.getClass().getCanonicalName()).append(": ");
        builder.append(throwable.getMessage());
        builder.append('\n');
        StackTraceElement[] trace = throwable.getStackTrace();
        for (StackTraceElement element : trace) {
            builder.append("\tat ").append(element).append('\n');
        }

        Set<Throwable> dejaVu = Collections.newSetFromMap(new IdentityHashMap<>());
        // Print suppressed exceptions, if any
        for (Throwable se : throwable.getSuppressed()) {
            printEnclosedStackTrace(builder, se, trace, "Suppressed: ", "\t", dejaVu);
        }

        // Print cause, if any
        Throwable cause = throwable.getCause();
        if (cause != null) {
            printEnclosedStackTrace(builder, cause, trace, "Caused by: ", "", dejaVu);
        }
    }

    private void printEnclosedStackTrace(final StringBuilder builder,
            final Throwable throwable,
            final StackTraceElement[] enclosingTrace,
            final String caption,
            final String prefix,
            final Set<Throwable> dejaVu) {
        if (dejaVu.contains(throwable)) {
            builder.append(prefix).append(caption).append("[CIRCULAR REFERENCE: ").append(this).append("]\n");
        } else {
            dejaVu.add(throwable);
            // Compute number of frames in common between this and enclosing trace
            StackTraceElement[] trace = throwable.getStackTrace();
            int m = trace.length - 1;
            int n = enclosingTrace.length - 1;
            while (m >= 0 && n >= 0 && trace[m].equals(enclosingTrace[n])) {
                m--;
                n--;
            }
            int framesInCommon = trace.length - 1 - m;

            // Print the stack trace
            builder.append(prefix).append(caption).append(throwable).append('\n');
            for (int i = 0; i <= m; i++) {
                builder.append(prefix).append("\tat ").append(trace[i]);
            }

            if (framesInCommon != 0) {
                builder.append(prefix).append("\t... ").append(framesInCommon).append(" more\n");
            }

            // Print suppressed exceptions, if any
            for (Throwable se : throwable.getSuppressed()) {
                printEnclosedStackTrace(builder, se, trace, "Suppressed: ", prefix + "\t", dejaVu);
            }

            // Print cause, if any
            Throwable cause = throwable.getCause();
            if (cause != null) {
                printEnclosedStackTrace(builder, cause, trace, "Caused by: ", prefix, dejaVu);
            }
        }
    }

    private String calculateDateString(final long milliseconds) {
        return dateTimeFormatter.format(Instant.ofEpochMilli(milliseconds));
    }

    private String getMessageColor(final LogRecord record) {
        if (record.getLevel().intValue() >= Level.SEVERE.intValue()) {
            return ANSI_RED;
        } else if (record.getLevel().intValue() >= Level.WARNING.intValue()) {
            return ANSI_YELLOW;
        } else {
            return ANSI_BLACK;
        }
    }
}
