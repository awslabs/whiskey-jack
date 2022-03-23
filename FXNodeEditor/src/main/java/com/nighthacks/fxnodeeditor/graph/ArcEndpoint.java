/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import javax.annotation.*;

public class ArcEndpoint {
    ArcEndpoint(@Nonnull MNode.Port m, @Nonnull FGNode c) {
        meta = m;
        container = c;
        uname = container.uid+":"+meta.name;
    }
    @Nonnull final MNode.Port meta;
    @Nonnull final FGNode container;
    final String uname;
    private FGNode.PortView view;
    public void setView(FGNode.PortView v) { view = v; }
    public FGNode.PortView getView() {
        return view;
    }
}
