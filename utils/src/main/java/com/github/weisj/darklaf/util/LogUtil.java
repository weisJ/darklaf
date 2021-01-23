/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.util;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.github.weisj.darklaf.log.DarkLogHandler;
import com.github.weisj.darklaf.log.LogFormatter;

public final class LogUtil {

    private static final Logger PARENT = Logger.getLogger("com.github.weisj.darklaf");
    private static final Handler LOG_HANDLER = new DarkLogHandler();

    static {
        LOG_HANDLER.setFormatter(new LogFormatter());
        PARENT.setUseParentHandlers(false);
        PARENT.addHandler(LOG_HANDLER);
    }

    public static Logger getLogger(final Class<?> clazz) {
        Logger logger = Logger.getLogger(clazz.getName());
        logger.setUseParentHandlers(true);
        return logger;
    }

    public static void setLevel(final Level level) {
        PARENT.setLevel(level);
        LOG_HANDLER.setLevel(level);
    }

    public static Level getLevel() {
        return PARENT.getLevel();
    }

    public static <T> T log(final T obj) {
        PARENT.info(String.valueOf(obj));
        return obj;
    }
}
