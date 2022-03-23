/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.nighthacks.fxnodeeditor.util;

public interface Commitable {
    void commit();

    void abandon();
}
