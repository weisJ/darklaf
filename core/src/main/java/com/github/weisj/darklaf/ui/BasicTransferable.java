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
package com.github.weisj.darklaf.ui;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringBufferInputStream;
import java.io.StringReader;
import java.util.logging.Logger;

import javax.swing.plaf.UIResource;

/**
 * A transferable implementation for the default data transfer of some Swing components.
 *
 * @author  Timothy Prinzing
 * @version 1.10 11/17/05
 */
public class BasicTransferable implements Transferable, UIResource {

    private static DataFlavor[] htmlFlavors;
    private static DataFlavor[] stringFlavors;
    private static DataFlavor[] plainFlavors;

    static {
        try {
            htmlFlavors = new DataFlavor[3];
            htmlFlavors[0] = new DataFlavor("text/html;class=java.lang.String");
            htmlFlavors[1] = new DataFlavor("text/html;class=java.io.Reader");
            htmlFlavors[2] = new DataFlavor("text/html;charset=unicode;class=java.io.InputStream");

            plainFlavors = new DataFlavor[3];
            plainFlavors[0] = new DataFlavor("text/plain;class=java.lang.String");
            plainFlavors[1] = new DataFlavor("text/plain;class=java.io.Reader");
            plainFlavors[2] = new DataFlavor("text/plain;charset=unicode;class=java.io.InputStream");

            stringFlavors = new DataFlavor[2];
            stringFlavors[0] = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=java.lang.String");
            stringFlavors[1] = DataFlavor.stringFlavor;

        } catch (ClassNotFoundException cle) {
            Logger.getGlobal().severe("error initializing javax.swing.plaf.basic.BasicTranserable");
        }
    }

    protected String plainData;
    protected String htmlData;

    public BasicTransferable(final String plainData, final String htmlData) {
        this.plainData = plainData;
        this.htmlData = htmlData;
    }

    /**
     * Returns an array of DataFlavor objects indicating the flavors the data can be provided in. The
     * array should be ordered according to preference for providing the data (from most richly
     * descriptive to least descriptive).
     *
     * @return an array of data flavors in which this data can be transferred.
     */
    public DataFlavor[] getTransferDataFlavors() {
        DataFlavor[] richerFlavors = getRicherFlavors();
        int nRicher = (richerFlavors != null) ? richerFlavors.length : 0;
        int nHTML = (isHTMLSupported()) ? htmlFlavors.length : 0;
        int nPlain = (isPlainSupported()) ? plainFlavors.length : 0;
        int nString = (isPlainSupported()) ? stringFlavors.length : 0;
        int nFlavors = nRicher + nHTML + nPlain + nString;
        DataFlavor[] flavors = new DataFlavor[nFlavors];

        // fill in the array
        int nDone = 0;
        if (nRicher > 0) {
            System.arraycopy(richerFlavors, 0, flavors, nDone, nRicher);
            nDone += nRicher;
        }
        if (nHTML > 0) {
            System.arraycopy(htmlFlavors, 0, flavors, nDone, nHTML);
            nDone += nHTML;
        }
        if (nPlain > 0) {
            System.arraycopy(plainFlavors, 0, flavors, nDone, nPlain);
            nDone += nPlain;
        }
        if (nString > 0) {
            System.arraycopy(stringFlavors, 0, flavors, nDone, nString);
            nDone += nString;
        }
        return flavors;
    }

    /**
     * Returns whether the specified data flavor is supported for this object.
     *
     * @param  flavor the requested flavor for the data
     * @return        boolean indicating whether the data flavor is supported.
     */
    public boolean isDataFlavorSupported(final DataFlavor flavor) {
        DataFlavor[] flavors = getTransferDataFlavors();
        for (DataFlavor dataFlavor : flavors) {
            if (dataFlavor.equals(flavor)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns an object which represents the data to be transferred. The class of the object returned
     * is defined by the representation class of the flavor.
     *
     * @param  flavor                     the requested flavor for the data
     * @throws UnsupportedFlavorException if the requested data flavor is not supported.
     * @see                               DataFlavor#getRepresentationClass
     */
    public Object getTransferData(final DataFlavor flavor) throws UnsupportedFlavorException {
        if (isRicherFlavor(flavor)) {
            return getRicherData(flavor);
        } else if (isHTMLFlavor(flavor)) {
            String data = getHTMLData();
            data = (data == null) ? "" : data;
            if (String.class.equals(flavor.getRepresentationClass())) {
                return data;
            } else if (Reader.class.equals(flavor.getRepresentationClass())) {
                return new StringReader(data);
            } else if (InputStream.class.equals(flavor.getRepresentationClass())) {
                return new StringBufferInputStream(data);
            }
            // fall through to unsupported
        } else if (isPlainFlavor(flavor)) {
            String data = getPlainData();
            data = (data == null) ? "" : data;
            if (String.class.equals(flavor.getRepresentationClass())) {
                return data;
            } else if (Reader.class.equals(flavor.getRepresentationClass())) {
                return new StringReader(data);
            } else if (InputStream.class.equals(flavor.getRepresentationClass())) {
                return new StringBufferInputStream(data);
            }
            // fall through to unsupported

        } else if (isStringFlavor(flavor)) {
            String data = getPlainData();
            data = (data == null) ? "" : data;
            return data;
        }
        throw new UnsupportedFlavorException(flavor);
    }

    // --- richer subclass flavors ----------------------------------------------

    protected boolean isRicherFlavor(final DataFlavor flavor) {
        DataFlavor[] richerFlavors = getRicherFlavors();
        int nFlavors = (richerFlavors != null) ? richerFlavors.length : 0;
        for (int i = 0; i < nFlavors; i++) {
            if (richerFlavors[i].equals(flavor)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Some subclasses will have flavors that are more descriptive than HTML or plain text. If this
     * method returns a non-null value, it will be placed at the start of the array of supported
     * flavors.
     *
     * @return the supported data flavours as array.
     */
    protected DataFlavor[] getRicherFlavors() {
        return null;
    }

    protected Object getRicherData(final DataFlavor flavor) {
        return null;
    }

    // --- html flavors ----------------------------------------------------------

    /**
     * Returns whether or not the specified data flavor is an HTML flavor that is supported.
     *
     * @param  flavor the requested flavor for the data
     * @return        boolean indicating whether or not the data flavor is supported
     */
    protected boolean isHTMLFlavor(final DataFlavor flavor) {
        DataFlavor[] flavors = htmlFlavors;
        for (DataFlavor dataFlavor : flavors) {
            if (dataFlavor.equals(flavor)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Should the HTML flavors be offered? If so, the method getHTMLData should be implemented to
     * provide something reasonable.
     *
     * @return true if html is supported.
     */
    protected boolean isHTMLSupported() {
        return htmlData != null;
    }

    /**
     * Fetch the data in a text/html format
     *
     * @return the html data.
     */
    protected String getHTMLData() {
        return htmlData;
    }

    // --- plain text flavors ----------------------------------------------------

    /**
     * Returns whether or not the specified data flavor is an plain flavor that is supported.
     *
     * @param  flavor the requested flavor for the data
     * @return        boolean indicating whether or not the data flavor is supported
     */
    protected boolean isPlainFlavor(final DataFlavor flavor) {
        DataFlavor[] flavors = plainFlavors;
        for (DataFlavor dataFlavor : flavors) {
            if (dataFlavor.equals(flavor)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Should the plain text flavors be offered? If so, the method getPlainData should be implemented to
     * provide something reasonable.
     *
     * @return true if plain text is supported.
     */
    protected boolean isPlainSupported() {
        return plainData != null;
    }

    /**
     * Fetch the data in a text/plain format.
     *
     * @return the plain data.
     */
    protected String getPlainData() {
        return plainData;
    }

    // --- string flavorss --------------------------------------------------------

    /**
     * Returns whether or not the specified data flavor is a String flavor that is supported.
     *
     * @param  flavor the requested flavor for the data
     * @return        boolean indicating whether or not the data flavor is supported
     */
    protected boolean isStringFlavor(final DataFlavor flavor) {
        DataFlavor[] flavors = stringFlavors;
        for (DataFlavor dataFlavor : flavors) {
            if (dataFlavor.equals(flavor)) {
                return true;
            }
        }
        return false;
    }
}
