//
// Copyright (C) 2013-2019 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Theme 1.0

// All Analysis forms must be built with the From QML item
Form
{
    usesJaspResults: true

    VariablesForm
    {
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList  { name: "target"     ; title: qsTr("Target")         ; singleVariable: true; allowedColumns: ["scale"]  }
        AssignedVariablesList  { name: "predictors" ; title: qsTr("Predictors")                                                        }
        AssignedVariablesList  { name: "weights"; title: qsTr("Weights"); singleVariable: true; allowedColumns: ["scale"]              }
    }

    GroupBox
    {

    title: qsTr("Penalty")

    RadioButtonGroup
    {
        name: "penalty"
        RadioButton { value: "ridge"      ; text: qsTr("Ridge")      ; checked: true   }
        RadioButton { value: "lasso"      ; text: qsTr("Lasso")      ; id: lasso       }
        RadioButton { value: "elasticNet" ; text: qsTr("Elastic Net"); id: elasticNet  }
    }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox { name: "regRegCoefTable";	text: qsTr("Coefficients table"); checked: true }
    }

    GroupBox {
        title: qsTr("Plots")

        CheckBox { name: "plotPredPerf"; text: qsTr("Predictive performance plot") }
        CheckBox { name: "plotCVLambda"; text: qsTr("λ evaluation plot")           }
    }

    ExpanderButton
    {
        title: qsTr("Training Parameters")

        GroupBox {
            DoubleField  { name: "alpha"      ; text: qsTr("α (elastic net only):")  ; defaultValue: 0.5 ; min: 0     ; max: 1; fieldWidth: 60; enabled: elasticNet.checked }
            DoubleField  { name: "thresh"     ; text: qsTr("Convergence threshold:") ; defaultValue: 1e-7; min: 1e-999; max: 1; fieldWidth: 60                              }
            PercentField { name: "dataTrain"  ; text: qsTr("Data used for trainnig:"); defaultValue: 80                                                                     }
            CheckBox     { name: "standardize"; text: qsTr("Standardize data")       ; checked: true                                                                        }
            CheckBox     { name: "intercept"  ; text: qsTr("Fit intercept")          ; checked: true                                                                        }
        }

        RadioButtonGroup {
            title: qsTr("λ (shrinkage)")
            name: "shrinkage"
            RadioButton { text: qsTr("Minimum CV MSE")  ; name: "optMin"; checked: true             }
            RadioButton { text: qsTr("Max. within 1 SE"); name: "opt1SE"                            }
            RadioButton { text: qsTr("Manual")          ; name: "manual"  ; childrenOnSameRow: true
                DoubleField { name: "lambda"; defaultValue: 1 ; min: 0; max: 999999; fieldWidth: 60 }
            }
        }

    }

    ExpanderButton
    {
        title: qsTr("Predictions")

        VariablesForm
        {
        AvailableVariablesList { name: "allVariablesApply" }
        AssignedVariablesList  { name: "indicator"  ; title: qsTr("Apply indicator"); singleVariable: true; allowedColumns: ["nominal"]; enabled: indicator.checked }
        }

        GroupBox
        {

        title: qsTr("Data predition")

        RadioButtonGroup
        {
            name: "applyModel"
            RadioButton { value: "noApp"         ; text: qsTr("Do not predict data")                ; checked: true }
            RadioButton { value: "applyIndicator"; text: qsTr("Predict data according to indicator"); id: indicator }
            RadioButton { value: "applyImpute"   ; text: qsTr("Predict missing values in target")                   }
        }
        }
    }

    ExpanderButton
    {
        title: qsTr("Advanced")

        GroupBox {
            CheckBox { name: "seedBox"; text: qsTr("Set seed:"); childrenOnSameRow: true
                DoubleField  { name: "seed"; defaultValue: 1; min: -999999; max: 999999; fieldWidth: 60 }
            }
        }
    }

}
