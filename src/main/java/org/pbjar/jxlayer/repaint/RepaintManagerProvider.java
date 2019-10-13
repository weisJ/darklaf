/*
 Copyright (c) 2009, Piet Blok
 All rights reserved.
 <p>
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 <p>
 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above
 copyright notice, this list of conditions and the following
 disclaimer in the documentation and/or other materials provided
 with the distribution.
 * Neither the name of the copyright holder nor the names of the
 contributors may be used to endorse or promote products derived
 from this software without specific prior written permission.
 <p>
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.pbjar.jxlayer.repaint;

import org.jdesktop.swingx.ForwardingRepaintManager;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

/**
 * To be implemented by classes that provide for a custom RepaintManager.
 *
 * @author Piet Blok
 * @see RepaintManagerUtils
 */
public interface RepaintManagerProvider {
    /**
     * Get the class of a {@link RepaintManager} that extends {@link ForwardingRepaintManager}.
     *
     * <p><b>Note:</b> the class must provide for a public constructor that takes a delegate {@link
     * RepaintManager} as its only argument.
     *
     * @return a class object
     */
    @NotNull
    Class<? extends ForwardingRepaintManager> getForwardingRepaintManagerClass();

    /**
     * Get the class of a {@link RepaintManager} that extends {@link WrappedRepaintManager}.
     *
     * <p><b>Note:</b> the class must provide for a public constructor that takes a delegate {@link
     * RepaintManager} as its only argument.
     *
     * @return a class object
     */
    @NotNull
    Class<? extends WrappedRepaintManager> getWrappedRepaintManagerClass();

    /**
     * Checks whether or not the argument class is a {@link RepaintManager} class that will do the
     * required job.
     *
     * @param rpm a {@link RepaintManager} class
     * @return {@code true} if the argument class will do the required job, {@code false} otherwise
     */
    boolean isAdequate(Class<? extends RepaintManager> rpm);
}
