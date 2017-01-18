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
	additive: boolean;
	trendSmoothness: number;
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
