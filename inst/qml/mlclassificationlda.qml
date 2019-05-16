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
        AssignedVariablesList  { name: "target"    ; title: qsTr("Target")         ; singleVariable: true; allowedColumns: ["nominal", "ordinal"] }
        AssignedVariablesList  { name: "predictors"; title: qsTr("Predictors")                                                                    }
        AssignedVariablesList  { name: "indicator" ; title: qsTr("Apply indicator"); singleVariable: true; allowedColumns: ["nominal"]            }
    }

    GridLayout {
        columns: 2

        ColumnLayout {

    GroupBox {
        title: qsTr("Tables")

        CheckBox { name: "classLdaConfTable";	    text: qsTr("Confusion matrix"); checked: true      }
        CheckBox { name: "classLdaPriorTable";	    text: qsTr("Prior probabilities of groups")            }
        CheckBox { name: "classLdaCoefloadTable";   text: qsTr("Coefficients linear discriminants")       }
        CheckBox { name: "classLdaMeanTable";       text: qsTr("Means training data")             }
              }


        }

    ColumnLayout {

    GroupBox {
        title: qsTr("Plots")

        CheckBox { name: "categories"; text: qsTr("Observations on linear discriminants"); checked: true }
        CheckBox { name: "roc"; text: qsTr("ROC") }
        CheckBox { name: "linearDiscr"; text: qsTr("Data on linear discriminants") }
        CheckBox { name: "errorRate"; text: qsTr("Error trade-off") }
        CheckBox { name: "territorial"; text: qsTr("Territorial map") }
        CheckBox { name: "partialPlot"; text: qsTr("Partial plots") }
             }
        }

    }

    ExpanderButton
    {
        title: qsTr("Training Parameters")

        GridLayout
        {


            RadioButtonGroup {
                title: qsTr("Model optimization")
                name: "modelOpt"
                RadioButton { text: qsTr("Leave-one-out cross validation") ; name: "validationLeaveOneOut"}
                RadioButton { text: qsTr("None") ; name: "noOpt"; checked: true }
            }

            GroupBox {

                PercentField { name: "dataTrain"; text: qsTr("Data used for training:")       ; defaultValue: 80    }
                DropDown {
                    name: "estimationMethod"
                    indexDefaultValue: 0
                    label: qsTr("Estimation method")
                    values:
                    [
                        { label: "Moment", value: "moment"},
                        { label: "MLE", value: "mle"},
                        { label: "MVE", value: "covMve"},
                        { label: "t", value: "robust"},
                    ]
                }

                CheckBox { name: "priorSetting"; text: qsTr("Set prior: "); childrenOnSameRow: true
                    DoubleField { name: "manualprior"; defaultValue: 0.5; min: 0; max: 1; fieldWidth: 60 }}
                CheckBox { name: "seedBox"; text: qsTr("Set seed: "); childrenOnSameRow: true
                        DoubleField  { name: "seed"; defaultValue: 1; min: -999999; max: 999999; fieldWidth: 60 }
            }

            }

           // RadioButtonGroup {
           //     name: "estimationMethod"
           //     title: "Estimation method"
           //     RadioButton { value: "moment";	text: qsTr("Moment"); checked: true }
           //     RadioButton { value: "mle";	text: qsTr("MLE") }
            //    RadioButton { value: "covMve"; text: qsTr("MVE") }
           //     RadioButton { value: "robust"; text: qsTr("t") }
          //  }


           // RadioButtonGroup {
           //     name: "priorSetting"
           //     title: "Prior"
           //     RadioButton { value: "classproptrain"; text: qsTr("Auto"); checked: true }
           //     RadioButton { value: "manual"; text: qsTr("Manual"); id: manualPrior }
           //     DoubleField
           //         {
           //            name: "manualprior"
           //            label: qsTr("Prior for each factor")
           //            defaultValue: 0.5
           //            enabled: manualPrior.checked
           //            indent: true
            //           min: 0
            //           max: 1
            //         }
           // }


        }
    }

    ExpanderButton
    {
        title: qsTr("Predictions")

        GroupBox {


            RadioButtonGroup
            {
                name: "applyModel"
                RadioButton { value: "noApp"         ; text: qsTr("Do not predict data"); checked: true        }
                RadioButton { value: "applyImpute"   ; text: qsTr("Predict missing values in target")  }
                RadioButton { value: "applyIndicator"; text: qsTr("Predict data according to apply indicator")       }

                   }
                }


    }

}

