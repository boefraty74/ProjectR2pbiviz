var powerbi;!function(t){var e;!function(t){var e;!function(t){var e;!function(t){function e(t,e,a,i){if(t){var s=t[e];if(s){var r=s[a];if(void 0!==r)return r}}return i}function a(t,e,a,i,s,r){if(t){var n=t[e];if(n){var o=n[a];if(o<s)return s;if(o>r)return r;if(void 0!==o)return o}}return i}function i(t,e,a,i,s,r){if(t){var n=t[e];if(n){var o=n[a];if(void 0!==o)return o>r?r:o<s?s:o}}return i}function s(t,e,a,i,s,r){return r&&e==a&&1==s?i:r&&e!=a&&0==s?i:t}function r(t,e){return"auto"!=e?"None":"auto"==e&&"None"==t?"fast":t}function n(t,e,a){return t<e?e:t>a?a:t}function o(t,e,a,i,s){var r=t.objects;if(r){var n=r[e];if(n){var o=n[a];if(o){var l=o[i];if(void 0!==l)return l}}}return s}function l(t,e,a,i){if(t){var s=t[e];if(s){var r=s[a];if(void 0!==r&&void 0!==r.solid&&void 0!==r.solid.color)return r.solid.color}}return i}t.getValue=e,t.getValueMinMax=a,t.getValueNumberMinMax=i,t.ifStringReturnString=s,t.ifStringReturnStringClustersMethod=r,t.inMinMax=n,t.getCategoricalObjectValue=o,t.getFillValue=l}(e=t.PBI_CV_CAD60C71_FAD5_4B59_A498_A7FB0DDC7A2B||(t.PBI_CV_CAD60C71_FAD5_4B59_A498_A7FB0DDC7A2B={}))}(e=t.visual||(t.visual={}))}(e=t.extensibility||(t.extensibility={}))}(powerbi||(powerbi={}));var powerbi;!function(t){var e;!function(t){var e;!function(t){var e;!function(t){var e=function(){function e(t){this.imageDiv=document.createElement("div"),this.imageDiv.className="rcv_autoScaleImageContainer",t.element.appendChild(this.imageDiv),this.imageElement=document.createElement("img"),this.imageElement.className="rcv_autoScaleImage",this.imageDiv.appendChild(this.imageElement),this.settings_model_params={modelType:"automatic",targetSeasonality:"autodetect from date",freq:12},this.settings_algo_params={degree:!1,robustToOutliers:!0,percentile:50},this.settings_plot_params={plotType:"all",weight:10,lineCol:"red",labelsCol:"orange",textSize:10},this.settings_extra_params={show:!0,textSize:8,infoCol:"gray"}}return e.prototype.update=function(e){var a=e.dataViews;if(a&&0!==a.length){var i=a[0];if(i&&i.metadata){this.settings_model_params={modelType:t.getValue(i.metadata.objects,"settings_model_params","modelType","automatic"),targetSeasonality:t.getValue(i.metadata.objects,"settings_model_params","targetSeasonality","autodetect from date"),freq:t.getValue(i.metadata.objects,"settings_model_params","freq",12)},this.settings_algo_params={degree:t.getValue(i.metadata.objects,"settings_algo_params","degree",!1),robustToOutliers:t.getValue(i.metadata.objects,"settings_algo_params","robustToOutliers",!0),percentile:t.getValue(i.metadata.objects,"settings_algo_params","percentile",50)},this.settings_plot_params={plotType:t.getValue(i.metadata.objects,"settings_plot_params","plotType","all"),weight:t.getValue(i.metadata.objects,"settings_plot_params","weight",10),lineCol:t.getValue(i.metadata.objects,"settings_plot_params","lineCol","red"),labelsCol:t.getValue(i.metadata.objects,"settings_plot_params","labelsCol","orange"),textSize:t.getValue(i.metadata.objects,"settings_plot_params","textSize",10)},this.settings_extra_params={show:t.getValue(i.metadata.objects,"settings_extra_params","show",!0),textSize:t.getValue(i.metadata.objects,"settings_extra_params","textSize",8),infoCol:t.getValue(i.metadata.objects,"settings_extra_params","infoCol","gray")};var s=null;i.scriptResult&&i.scriptResult.payloadBase64&&(s="data:image/png;base64,"+i.scriptResult.payloadBase64),s?this.imageElement.src=s:this.imageElement.src=null,this.onResizing(e.viewport)}}},e.prototype.onResizing=function(t){this.imageDiv.style.height=t.height+"px",this.imageDiv.style.width=t.width+"px"},e.prototype.enumerateObjectInstances=function(e){var a=e.objectName,i=[];switch(a){case"settings_model_params":i.push({objectName:a,properties:{modelType:this.settings_model_params.modelType,targetSeasonality:this.settings_model_params.targetSeasonality,freq:t.inMinMax(this.settings_model_params.freq,1,1e4)},selector:null});break;case"settings_algo_params":i.push({objectName:a,properties:{degree:this.settings_algo_params.degree,robustToOutliers:this.settings_algo_params.robustToOutliers,percentile:t.inMinMax(this.settings_algo_params.percentile,1,100)},selector:null});break;case"settings_plot_params":i.push({objectName:a,properties:{plotType:this.settings_plot_params.plotType,weight:t.inMinMax(this.settings_plot_params.weight,1,50),lineCol:this.settings_plot_params.lineCol,labelsCol:this.settings_plot_params.labelsCol,textSize:t.inMinMax(this.settings_plot_params.textSize,8,40)},selector:null});break;case"settings_extra_params":i.push({objectName:a,properties:{show:this.settings_extra_params.show,textSize:t.inMinMax(this.settings_extra_params.textSize,8,40),infoCol:this.settings_extra_params.infoCol},selector:null})}return i},e}();t.Visual=e}(e=t.PBI_CV_CAD60C71_FAD5_4B59_A498_A7FB0DDC7A2B||(t.PBI_CV_CAD60C71_FAD5_4B59_A498_A7FB0DDC7A2B={}))}(e=t.visual||(t.visual={}))}(e=t.extensibility||(t.extensibility={}))}(powerbi||(powerbi={}));var powerbi;!function(t){var e;!function(e){var a;!function(e){e.PBI_CV_CAD60C71_FAD5_4B59_A498_A7FB0DDC7A2B={name:"PBI_CV_CAD60C71_FAD5_4B59_A498_A7FB0DDC7A2B",displayName:"Time series decomposition",class:"Visual",version:"1.0.0",apiVersion:"1.3.0",create:function(e){return new t.extensibility.visual.PBI_CV_CAD60C71_FAD5_4B59_A498_A7FB0DDC7A2B.Visual(e)},custom:!0}}(a=e.plugins||(e.plugins={}))}(e=t.visuals||(t.visuals={}))}(powerbi||(powerbi={}));