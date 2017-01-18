var powerbi;
(function (powerbi) {
    var extensibility;
    (function (extensibility) {
        var visual;
        (function (visual) {
            var PBI_CV_EXAMPLE1;
            (function (PBI_CV_EXAMPLE1) {
                /**
                 * Gets property value for a particular object.
                 *
                 * @function
                 * @param {DataViewObjects} objects - Map of defined objects.
                 * @param {string} objectName       - Name of desired object.
                 * @param {string} propertyName     - Name of desired property.
                 * @param {T} defaultValue          - Default value of desired property.
                 */
                function getValue(objects, objectName, propertyName, defaultValue) {
                    if (objects) {
                        var object = objects[objectName];
                        if (object) {
                            var property = object[propertyName];
                            if (property !== undefined) {
                                return property;
                            }
                        }
                    }
                    return defaultValue;
                }
                PBI_CV_EXAMPLE1.getValue = getValue;
                /**
                 * Gets property value for a particular object in certain range
                 *  UNDER DEVELOPMENT
                 * @function
                 * @param {DataViewObjects} objects - Map of defined objects.
                 * @param {string} objectName       - Name of desired object.
                 * @param {string} propertyName     - Name of desired property.
                 * @param {T} defaultValue          - Default value of desired property.
                 */
                function getValueMinMax(objects, objectName, propertyName, defaultValue, minVal, maxVal) {
                    if (objects) {
                        var object = objects[objectName];
                        if (object) {
                            var property = object[propertyName];
                            if (property < minVal) {
                                return minVal;
                            }
                            if (property > maxVal) {
                                return maxVal;
                            }
                            if (property !== undefined) {
                                return property;
                            }
                        }
                    }
                    return defaultValue;
                }
                PBI_CV_EXAMPLE1.getValueMinMax = getValueMinMax;
                /**
                * Gets property value for a particular object of numerical type
                *
                * @function
                * @param {DataViewObjects} objects - Map of defined objects.
                * @param {string} objectName       - Name of desired object.
                * @param {string} propertyName     - Name of desired property.
                * @param {T} defaultValue          - Default value of desired property.
                */
                function getValueNumberMinMax(objects, objectName, propertyName, defaultValue, minValue, maxValue) {
                    if (objects) {
                        var object = objects[objectName];
                        if (object) {
                            var property = object[propertyName];
                            if (property !== undefined) {
                                if (property > maxValue) {
                                    return maxValue;
                                }
                                if (property < minValue) {
                                    return minValue;
                                }
                                return property;
                            }
                        }
                    }
                    return defaultValue;
                }
                PBI_CV_EXAMPLE1.getValueNumberMinMax = getValueNumberMinMax;
                /**
                     * Gets conditional property value for a particular object of type string
                     *
                     * @function
                     * @param {string} inVal     -  current value of parameter
                     * @param {string} contrVal   - control value
                     * @param {string} contrVal2Compare     - specific string to be compared with contrVal
                     * @param {boolean} logic          -  true / false "logic"
                     * @param {string} outValIfCondTrue          - output value if comparison (contrVal == contrVal2Compare) comes out as "logic"
                     */
                function ifStringReturnString(inVal, contrVal, contrVal2Compare, outValIfCondTrue, logic, applyNow) {
                    if (applyNow && contrVal == contrVal2Compare && logic == true)
                        return outValIfCondTrue;
                    if (applyNow && contrVal != contrVal2Compare && logic == false)
                        return outValIfCondTrue;
                    return inVal;
                }
                PBI_CV_EXAMPLE1.ifStringReturnString = ifStringReturnString;
                function ifStringReturnStringClustersMethod(numClustersMethods, numOfClusters) {
                    if (numOfClusters != "auto")
                        return "None";
                    if (numOfClusters == "auto" && numClustersMethods == "None")
                        return "fast";
                    return numClustersMethods;
                }
                PBI_CV_EXAMPLE1.ifStringReturnStringClustersMethod = ifStringReturnStringClustersMethod;
                function inMinMax(a, mi, ma) {
                    if (a < mi)
                        return mi;
                    if (a > ma)
                        return ma;
                    return a;
                }
                PBI_CV_EXAMPLE1.inMinMax = inMinMax;
                /**
                 * Gets property value for a particular object in a category.
                 *
                 * @function
                 * @param {DataViewCategoryColumn} category - List of category objects.
                 * @param {number} index                    - Index of category object.
                 * @param {string} objectName               - Name of desired object.
                 * @param {string} propertyName             - Name of desired property.
                 * @param {T} defaultValue                  - Default value of desired property.
                 */
                function getCategoricalObjectValue(category, index, objectName, propertyName, defaultValue) {
                    var categoryObjects = category.objects;
                    if (categoryObjects) {
                        var categoryObject = categoryObjects[index];
                        if (categoryObject) {
                            var object = categoryObject[objectName];
                            if (object) {
                                var property = object[propertyName];
                                if (property !== undefined) {
                                    return property;
                                }
                            }
                        }
                    }
                    return defaultValue;
                }
                PBI_CV_EXAMPLE1.getCategoricalObjectValue = getCategoricalObjectValue;
                function getFillValue(objects, objectName, propertyName, defaultValue) {
                    if (objects) {
                        var object = objects[objectName];
                        if (object) {
                            var fill = object[propertyName];
                            if (fill !== undefined && fill.solid !== undefined && fill.solid.color !== undefined) {
                                return fill.solid.color;
                            }
                        }
                    }
                    return defaultValue;
                }
                PBI_CV_EXAMPLE1.getFillValue = getFillValue;
            })(PBI_CV_EXAMPLE1 = visual.PBI_CV_EXAMPLE1 || (visual.PBI_CV_EXAMPLE1 = {}));
        })(visual = extensibility.visual || (extensibility.visual = {}));
    })(extensibility = powerbi.extensibility || (powerbi.extensibility = {}));
})(powerbi || (powerbi = {}));
/*
 *  Power BI Visual CLI
 *
 *  Copyright (c) Microsoft Corporation
 *  All rights reserved.
 *  MIT License
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the ""Software""), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */
var powerbi;
(function (powerbi) {
    var extensibility;
    (function (extensibility) {
        var visual;
        (function (visual) {
            var PBI_CV_EXAMPLE1;
            (function (PBI_CV_EXAMPLE1) {
                /*   interface VisualSettingsVizParams {//appearance
                     show: boolean;
                     drawEllipse: boolean;
                     drawConvexHull: boolean;
                     drawCentroid: boolean;
                     percentile: number; //TODO: percentage
                     weight: number;
                  }
              */
                var Visual = (function () {
                    function Visual(options) {
                        this.imageDiv = document.createElement('div');
                        this.imageDiv.className = 'rcv_autoScaleImageContainer';
                        options.element.appendChild(this.imageDiv);
                        this.imageElement = document.createElement('img');
                        this.imageElement.className = 'rcv_autoScaleImage';
                        this.imageDiv.appendChild(this.imageElement);
                        /* this.settings_viz_params = <VisualSettingsVizParams>{
                            show: false,
                            drawEllipse: false,
                            drawConvexHull: false,
                            drawCentroid: false,
                            percentile: 40,
                            weight: 10,
                        }; */
                        //PBI_TEMPLATE_3: groups of parameters and defaults
                        this.settings_model_params = {
                            modelType: "automatic",
                            targetSeasonality: "autodetect from date",
                            freq: 12,
                        };
                        this.settings_algo_params = {
                            degree: false,
                            robustToOutliers: true,
                            percentile: 50,
                        };
                        this.settings_plot_params = {
                            plotType: "all",
                            weight: 10,
                            lineCol: "red",
                            labelsCol: "orange",
                            textSize: 10,
                        };
                        this.settings_extra_params = {
                            show: true,
                            textSize: 8,
                            infoCol: "gray",
                        };
                    }
                    Visual.prototype.update = function (options) {
                        var dataViews = options.dataViews;
                        if (!dataViews || dataViews.length === 0)
                            return;
                        var dataView = dataViews[0];
                        if (!dataView || !dataView.metadata)
                            return;
                        //PBI_TEMPLATE_4: groups of parameters populate values 
                        this.settings_model_params = {
                            modelType: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_model_params', 'modelType', "automatic"),
                            targetSeasonality: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_model_params', 'targetSeasonality', "autodetect from date"),
                            freq: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_model_params', 'freq', 12),
                        };
                        this.settings_algo_params = {
                            degree: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_algo_params', 'degree', false),
                            robustToOutliers: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_algo_params', 'robustToOutliers', true),
                            percentile: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_algo_params', 'percentile', 50),
                        };
                        this.settings_plot_params = {
                            plotType: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_plot_params', 'plotType', "all"),
                            weight: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_plot_params', 'weight', 10),
                            lineCol: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_plot_params', 'lineCol', "red"),
                            labelsCol: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_plot_params', 'labelsCol', "orange"),
                            textSize: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_plot_params', 'textSize', 10),
                        };
                        this.settings_extra_params = {
                            show: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_extra_params', 'show', true),
                            textSize: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_extra_params', 'textSize', 8),
                            infoCol: PBI_CV_EXAMPLE1.getValue(dataView.metadata.objects, 'settings_extra_params', 'infoCol', "gray"),
                        };
                        /* this.settings_viz_params = <VisualSettingsVizParams> {
                            show: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'show', false),
                            drawEllipse: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawEllipse', false),
                            drawConvexHull: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawConvexHull', false),
                            drawCentroid: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawCentroid', false),
                            percentile: getValue<number>(dataView.metadata.objects, 'settings_viz_params', 'percentile',40),
                            weight: getValue<number>(dataView.metadata.objects, 'settings_viz_params', 'weight',10),
                        }; */
                        var imageUrl = null;
                        if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                            imageUrl = "data:image/png;base64," + dataView.scriptResult.payloadBase64;
                        }
                        if (imageUrl) {
                            this.imageElement.src = imageUrl;
                        }
                        else {
                            this.imageElement.src = null;
                        }
                        this.onResizing(options.viewport);
                    };
                    Visual.prototype.onResizing = function (finalViewport) {
                        this.imageDiv.style.height = finalViewport.height + 'px';
                        this.imageDiv.style.width = finalViewport.width + 'px';
                    };
                    Visual.prototype.enumerateObjectInstances = function (options) {
                        var objectName = options.objectName;
                        var objectEnumeration = [];
                        switch (objectName) {
                            /* case 'settings_labeling_params':
                            objectEnumeration.push({
                                objectName: objectName,
                                properties: {
                                    show: this.settings_labeling_params.show,
                                    textSize: this.settings_labeling_params.textSize,
                                    percentile: this.settings_labeling_params.percentile,
                                    maxLenPointLabel: inMinMax(this.settings_labeling_params.maxLenPointLabel, 1, 100),
                                    percentile1: inMinMax(this.settings_labeling_params.percentile1, 0, 100)
                                 },
                                selector: null
                            });
                            break; */
                            //PBI_TEMPLATE_5: populate switch cases
                            case 'settings_model_params':
                                objectEnumeration.push({
                                    objectName: objectName,
                                    properties: {
                                        modelType: this.settings_model_params.modelType,
                                        targetSeasonality: this.settings_model_params.targetSeasonality,
                                        freq: PBI_CV_EXAMPLE1.inMinMax(this.settings_model_params.freq, 1, 10000),
                                    },
                                    selector: null
                                });
                                break;
                            case 'settings_algo_params':
                                objectEnumeration.push({
                                    objectName: objectName,
                                    properties: {
                                        degree: this.settings_algo_params.degree,
                                        robustToOutliers: this.settings_algo_params.robustToOutliers,
                                        percentile: PBI_CV_EXAMPLE1.inMinMax(this.settings_algo_params.percentile, 1, 100),
                                    },
                                    selector: null
                                });
                                break;
                            case 'settings_plot_params':
                                objectEnumeration.push({
                                    objectName: objectName,
                                    properties: {
                                        plotType: this.settings_plot_params.plotType,
                                        weight: PBI_CV_EXAMPLE1.inMinMax(this.settings_plot_params.weight, 1, 50),
                                        lineCol: this.settings_plot_params.lineCol,
                                        labelsCol: this.settings_plot_params.labelsCol,
                                        textSize: PBI_CV_EXAMPLE1.inMinMax(this.settings_plot_params.textSize, 8, 40),
                                    },
                                    selector: null
                                });
                                break;
                            case 'settings_extra_params':
                                objectEnumeration.push({
                                    objectName: objectName,
                                    properties: {
                                        show: this.settings_extra_params.show,
                                        textSize: PBI_CV_EXAMPLE1.inMinMax(this.settings_extra_params.textSize, 8, 40),
                                        infoCol: this.settings_extra_params.infoCol,
                                    },
                                    selector: null
                                });
                                break;
                        }
                        ;
                        return objectEnumeration;
                    };
                    return Visual;
                }());
                PBI_CV_EXAMPLE1.Visual = Visual;
            })(PBI_CV_EXAMPLE1 = visual.PBI_CV_EXAMPLE1 || (visual.PBI_CV_EXAMPLE1 = {}));
        })(visual = extensibility.visual || (extensibility.visual = {}));
    })(extensibility = powerbi.extensibility || (powerbi.extensibility = {}));
})(powerbi || (powerbi = {}));
var powerbi;
(function (powerbi) {
    var visuals;
    (function (visuals) {
        var plugins;
        (function (plugins) {
            plugins.PBI_CV_EXAMPLE1 = {
                name: 'PBI_CV_EXAMPLE1',
                displayName: 'Time series decomposition',
                class: 'Visual',
                version: '1.0.0',
                apiVersion: '1.3.0',
                create: function (options) { return new powerbi.extensibility.visual.PBI_CV_EXAMPLE1.Visual(options); },
                custom: true
            };
        })(plugins = visuals.plugins || (visuals.plugins = {}));
    })(visuals = powerbi.visuals || (powerbi.visuals = {}));
})(powerbi || (powerbi = {}));
//# sourceMappingURL=visual.js.map