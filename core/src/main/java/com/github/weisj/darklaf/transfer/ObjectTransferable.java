/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.transfer;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;

public class ObjectTransferable<T> implements Transferable {

    private final DataFlavor flavor;
    private final T value;

    public ObjectTransferable(final T value, final Class<T> type) {
        this.value = value;
        try {
            flavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=" + type.getName());
        } catch (final ClassNotFoundException e) {
            throw new IllegalArgumentException(e);
        }
    }

    @Override
    public DataFlavor[] getTransferDataFlavors() {
        return new DataFlavor[] {flavor};
    }

    @Override
    public boolean isDataFlavorSupported(final DataFlavor flavor) {
        return this.flavor.equals(flavor);
    }

    @Override
    public Object getTransferData(final DataFlavor flavor) {
        return value;
    }
}
