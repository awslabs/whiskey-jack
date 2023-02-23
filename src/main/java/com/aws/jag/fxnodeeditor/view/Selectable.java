/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.jag.fxnodeeditor.view;

public interface Selectable {
    public javafx.scene.Node getView();
    public void delete();
}
