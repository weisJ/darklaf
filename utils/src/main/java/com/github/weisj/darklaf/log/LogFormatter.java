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
package com.github.weisj.darklaf.log;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import com.github.weisj.darklaf.util.StringUtil;

/** @author Jannis Weis */
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
        return builder.toString();
    }

    private String calculateDateString(final long milliseconds) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Date date = new Date(milliseconds);
        return dateFormat.format(date);
    }

    private String getMessageColor(final LogRecord record) {
        if (record.getLevel() == Level.SEVERE) {
            return ANSI_RED;
        } else if (record.getLevel() == Level.WARNING) {
            return ANSI_YELLOW;
        } else {
            return ANSI_BLACK;
        }
    }
}
