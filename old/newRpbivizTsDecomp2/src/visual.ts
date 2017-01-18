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
module powerbi.extensibility.visual {
    
//PBI_TEMPLATE_1: interface definition blocks
interface Visual_settings_tsdecomp_params {
	show: boolean;
	freq: number;
	degree: boolean;
	robustToOutliers: boolean;
	model: string;
	percentile: number;
	weight: number;
	drawWhat: string;
}

   
   /*   interface VisualSettingsVizParams {//appearance 
        show: boolean;
        drawEllipse: boolean;
        drawConvexHull: boolean;
        drawCentroid: boolean;
        percentile: number; //TODO: percentage
        weight: number;   
     }
 */   

    

    export class Visual implements IVisual {
        private imageDiv: HTMLDivElement;
        private imageElement: HTMLImageElement;

//        private settings_prepocessing_params: VisualSettingsPreprocessingParams;
//PBI_TEMPLATE_2: declare groups of parameters
private settings_tsdecomp_params: Visual_settings_tsdecomp_params;
     

        public constructor(options: VisualConstructorOptions) {
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
this.settings_tsdecomp_params = <Visual_settings_tsdecomp_params> {
	show: true,
	freq: 12,
	degree: false,
	robustToOutliers: true,
	model: "additive",
	percentile: 10,
	weight: 10,
	drawWhat: "all",
};



            
        }

        public update(options: VisualUpdateOptions) {
            let dataViews: DataView[] = options.dataViews;
            if (!dataViews || dataViews.length === 0)
                return;

            let dataView: DataView = dataViews[0];
            if (!dataView || !dataView.metadata)
                return;

//PBI_TEMPLATE_4: groups of parameters populate values 
this.settings_tsdecomp_params = <Visual_settings_tsdecomp_params> {
	show: getValue <boolean>( dataView.metadata.objects, 'settings_tsdecomp_params', 'show', true),
	freq: getValue <number>( dataView.metadata.objects, 'settings_tsdecomp_params', 'freq', 12),
	degree: getValue <boolean>( dataView.metadata.objects, 'settings_tsdecomp_params', 'degree', false),
	robustToOutliers: getValue <boolean>( dataView.metadata.objects, 'settings_tsdecomp_params', 'robustToOutliers', true),
	model: getValue <string>( dataView.metadata.objects, 'settings_tsdecomp_params', 'model', "additive"),
	percentile: getValue <number>( dataView.metadata.objects, 'settings_tsdecomp_params', 'percentile', 10),
	weight: getValue <number>( dataView.metadata.objects, 'settings_tsdecomp_params', 'weight', 10),
	drawWhat: getValue <string>( dataView.metadata.objects, 'settings_tsdecomp_params', 'drawWhat', "all"),
};

			/* this.settings_viz_params = <VisualSettingsVizParams> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'show', false),
                drawEllipse: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawEllipse', false),
                drawConvexHull: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawConvexHull', false),
                drawCentroid: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawCentroid', false),
                percentile: getValue<number>(dataView.metadata.objects, 'settings_viz_params', 'percentile',40),
                weight: getValue<number>(dataView.metadata.objects, 'settings_viz_params', 'weight',10),
            }; */

               
           

            let imageUrl: string = null;
            if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                imageUrl = "data:image/png;base64," + dataView.scriptResult.payloadBase64;
            }

            if (imageUrl) {
                this.imageElement.src = imageUrl;
            } else {
                this.imageElement.src = null;
            }

            this.onResizing(options.viewport);
        }

        public onResizing(finalViewport: IViewport): void {
            this.imageDiv.style.height = finalViewport.height + 'px';
            this.imageDiv.style.width = finalViewport.width + 'px';
        }

        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions): VisualObjectInstanceEnumeration {
            let objectName = options.objectName;
            let objectEnumeration = [];

            switch(objectName) {
                
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
	case 'settings_tsdecomp_params':
	objectEnumeration.push({ 
 	 objectName: objectName,
 		  properties: { 
	show: this.settings_tsdecomp_params.show,
	freq: inMinMax(this.settings_tsdecomp_params.freq, 2, 10000),
	degree: this.settings_tsdecomp_params.degree,
	robustToOutliers: this.settings_tsdecomp_params.robustToOutliers,
	model: this.settings_tsdecomp_params.model,
	percentile: inMinMax(this.settings_tsdecomp_params.percentile, 0, 100),
	weight: inMinMax(this.settings_tsdecomp_params.weight, 0, 50),
	drawWhat: this.settings_tsdecomp_params.drawWhat,
},
 selector: null 
 }); 
 break;
            };

            return objectEnumeration;
        }
    }
}
