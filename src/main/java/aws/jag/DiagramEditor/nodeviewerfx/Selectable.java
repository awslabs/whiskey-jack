/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.jag.DiagramEditor.nodeviewerfx;

public interface Selectable {
    public javafx.scene.Node getView();
    public void delete();
}
