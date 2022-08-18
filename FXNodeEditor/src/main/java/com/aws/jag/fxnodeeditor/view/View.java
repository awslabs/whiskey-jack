/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.*;

public class View extends Graph<NodeView,PortView,ArcView,View> {
    public View() {
        super(NodeView.class, PortView.class, ArcView.class);
    }
    @Override
    public String getName() {
        return "View";
    }
    @Override
    public String toString() {
        return getName();
    }
    public void adjustArcs() {
        //TODO implement
    }
}
