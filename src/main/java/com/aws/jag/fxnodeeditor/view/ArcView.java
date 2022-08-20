/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.*;

class ArcView extends Arc {
    @SuppressWarnings("unused")
    public ArcView(Port A, Port B) {
        super(A, B);
    }
    @Override
    public View getContext() {
        return (View)super.getContext();
    }
}
