/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.WhiskeyJack.code;

import aws.WhiskeyJack.code.OverallCodeGenerationDriver.StrategyPath;
import aws.WhiskeyJack.exl.*;


public interface DomainGenerationController {
    default void prescan(){}
    public void setStrategyPath(StrategyPath p);
    public void generate(DomainCode nodes);
    default void close(){}
}
