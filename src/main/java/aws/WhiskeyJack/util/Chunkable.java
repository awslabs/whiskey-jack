/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.WhiskeyJack.util;

import java.util.List;

public interface Chunkable<T> {
    void setVariablePayload(List<T> variablePayload);
}