import React, { useState, useEffect, useRef } from 'react';
import { Button, Card, message, Space } from 'antd';
import { PageContainer } from '@ant-design/pro-layout';
import { ECharts } from 'echarts';
import * as echarts from 'echarts';
import { PlayCircleOutlined } from '@ant-design/icons';
import { once } from '@/services/eastx-sap/stock';

/**
 * The colors for the chart
 */
const upColor = '#ec0000';
const downColor = '#00da3c';

const buyColor = '#ef232a';
const sellColor = '#14b143';

/**
 * 请求数据
 *
 * @param fields
 */
 const handleStock = async () : Promise<[any, any]> => {
    const hide = message.loading('正在请求');

    const [err, data] = await once({});

    hide();

    return [err, data];
};

/**
 * Echarts Options
 */
interface ChartOption {
    title: string;
    categoryData: string[];
    candlestickData: Array<number[]>;
    volumeData: Array<number[]>;
    markPointData: any[];
};

/**
 * 分数据
 * @param stock 
 * 
 * @returns 
 */
const splitData = (response: API.DataReturn) : ChartOption => {
    const stock = response.data;
    const datetimes: string[] = [];
    const prices = Array<number[]>();
    const volumes = Array<number[]>();
    let markPoints: any[] = [];
    
    let i=0;

    stock.series.forEach((bar: API.Bar) => {
        //时间序列
        datetimes.push(bar.datetime);

        //数据意义：开盘(open)，收盘(close)，最低(lowest)，最高(highest)
        prices.push([bar.price.open, bar.price.close, bar.price.low, bar.price.high])

        //成交量
        volumes.push([i++, bar.volume, bar.price.close > bar.price.open ? 1 : -1]);

        //标记
        if(bar.signal.direction === "B") {
            markPoints.push(buyPoint(bar.datetime, bar.price.high, bar.signal.amount));
        } else if (bar.signal.direction === "S") {
            markPoints.push(sellPoint(bar.datetime, bar.price.high, bar.signal.amount));
        }
    });

    return {
        title: stock.code, 
        categoryData: datetimes, 
        candlestickData: prices, 
        volumeData: volumes,
        markPointData: markPoints
    };
};

/**
 * 
 * @param xAxis 
 * @param yAxis 
 * @param amount 
 * @returns 
 */
const buyPoint = (xAxis: string, yAxis: number, amount: number) : {} => {
    return markPoint(xAxis, yAxis, buyColor, amount, "Buy Point", "B");
};

/**
 * 
 * @param xAxis 
 * @param yAxis 
 * @param amount 
 * @returns 
 */
const sellPoint = (xAxis: string, yAxis: number, amount: number) : {} => {
    return markPoint(xAxis, yAxis, sellColor, amount, "Sell Point", "S");
};

/**
 * 
 * @param xAxis 
 * @param yAxis 
 * @param theColor 
 * @param theValue 
 * @param theName 
 * @param theLabel 
 * @returns 
 */
const markPoint = (xAxis: string, yAxis: number, theColor: string, theValue: number, theName: string, theLabel: string) : {} => {
    return {
        name: theName,
        coord: [xAxis, yAxis],
        //symbol: 'none',
        symbolSize: 40,
        symbolOffset: [0, -2],
        value: theValue,
        itemStyle: {
            color: theColor,
            opacity: 0.8
        },
        label:{
            formatter: theLabel
        }
    };
};

/**
 * 计算均线
 * @param sData 
 * @param dayCount 
 * @returns 
 */
function calculateMA(dayCount: number, sData: Array<number[]>) {
    var result = [];
    for (var i = 0, len = sData.length; i < len; i++) {
        if (i < dayCount) {
            result.push('-');
            continue;
        }
        var sum = 0;
        for (var j = 0; j < dayCount; j++) {
            sum += sData[i - j][3];
        }

        result.push((sum/dayCount).toFixed(2));
    }
    return result;
}

/**
 * 
 * @param data 
 * @returns 
 */
const getOption = (data : ChartOption) => {
    const option = {
        //动画开关
        animation: false,
        //标题
        title: {
            text: data.title,
            left: 0
        },
        //图例
        legend: {
            data: ['Day', 'MA5', 'MA10', 'MA20', 'MA30']
        },
        //弹出框
        tooltip: {
            trigger: 'axis',
            axisPointer: {
                type: 'cross'
            },
            borderWidth: 1,
            borderColor: '#ccc',
            padding: 10,
            textStyle: {
                color: '#000'
            },
            // position: function (pos, params, el, elRect, size) {
            //     var obj = {top: 10};
            //     obj[['left', 'right'][+(pos[0] < size.viewSize[0] / 2)]] = 30;
            //     return obj;
            // }
            // extraCssText: 'width: 170px'
        },
        //选择框
        toolbox: {
            feature: {
                dataZoom: {
                    yAxisIndex: false
                },
                brush: {
                    type: ['lineX', 'clear']
                }
            }
        },
        //
        axisPointer: {
            link: {xAxisIndex: 'all'},
            label: {
                backgroundColor: '#777'
            }
        },
        brush: {
            xAxisIndex: 'all',
            brushLink: 'all',
            outOfBrush: {
                colorAlpha: 0.1
            }
        },
        visualMap: {
            show: false,
            seriesIndex: 5,
            dimension: 2,
            pieces: [{
                value: 1,
                color: downColor
            }, {
                value: -1,
                color: upColor
            }]
        },
        grid: [            
            {
                left: '10%',
                right: '8%',
                height: '50%'
            },
            {
                left: '10%',
                right: '8%',
                top: '63%',
                height: '16%'
            }
        ],
        //横坐标
        xAxis: [            
            {
                type: 'category',
                data: data.categoryData,
                scale: true,
                boundaryGap: false,
                axisLine: {onZero: false},
                splitLine: {show: false},
                splitNumber: 20,
                min: 'dataMin',
                max: 'dataMax',
                axisPointer: {
                    z: 100
                }
            },
            {
                type: 'category',
                gridIndex: 1,
                data: data.categoryData,
                scale: true,
                boundaryGap: false,
                axisLine: {onZero: false},
                axisTick: {show: false},
                splitLine: {show: false},
                axisLabel: {show: false},
                splitNumber: 20,
                min: 'dataMin',
                max: 'dataMax'
            }
        ],
        //纵坐标
        yAxis: [
            {
                scale: true,
                splitArea: {
                    show: true
                }
            },
            {
                scale: true,
                gridIndex: 1,
                splitNumber: 2,
                axisLabel: {show: false},
                axisLine: {show: false},
                axisTick: {show: false},
                splitLine: {show: false}
            }
        ],
        //数据缩略
        dataZoom: [
            {
                type: 'inside',
                xAxisIndex: [0, 1],
                start: 60,
                end: 100
            },
            {
                show: true,
                xAxisIndex: [0, 1],
                type: 'slider',
                top: '85%',
                start: 60,
                end: 100
            }
        ],
        //纵坐标数据序列
        series: [
            {
                name: 'Day',
                type: 'candlestick',
                data: data.candlestickData,
                itemStyle: {
                    color: upColor,
                    color0: downColor,
                    borderColor: null,
                    borderColor0: null
                },
                tooltip: {
                    formatter: function (param) {
                        param = param[0];
                        return [
                            'Date: ' + param.name + '<hr size=1 style="margin: 3px 0">',
                            'Open: ' + param.data[0] + '<br/>',
                            'Close: ' + param.data[1] + '<br/>',
                            'Lowest: ' + param.data[2] + '<br/>',
                            'Highest: ' + param.data[3] + '<br/>'
                        ].join('');
                    }
                },
                //标记点
                markPoint: {
                    label: {
                        normal: {
                            formatter: function (param: { value: number; } | null) {
                                return param != null ? Math.round(param.value) : '';
                            }
                        }
                    },
                    data: data.markPointData.concat([
                        {
                            name: 'highest value',
                            type: 'max',
                            valueDim: 'highest'
                        },
                        {
                            name: 'lowest value',
                            type: 'min',
                            valueDim: 'lowest'
                        },
                        {
                            name: 'average value on close',
                            type: 'average',
                            valueDim: 'close'
                        },
                    ]),
                    tooltip: {
                        formatter: function (param: { name: string; data: { coord: any; }; }) {
                            return param.name + '<br>' + (param.data.coord || '');
                        }
                    }
                },
                //标记线
                markLine: {
                    symbol: ['none', 'none'],
                    data: [
                        [
                            {
                                name: 'from lowest to highest',
                                type: 'min',
                                valueDim: 'lowest',
                                symbol: 'circle',
                                symbolSize: 10,
                                label: {
                                    show: false
                                },
                                emphasis: {
                                    label: {
                                        show: false
                                    }
                                }
                            },
                            {
                                type: 'max',
                                valueDim: 'highest',
                                symbol: 'circle',
                                symbolSize: 10,
                                label: {
                                    show: false
                                },
                                emphasis: {
                                    label: {
                                        show: false
                                    }
                                }
                            }
                        ],
                        {
                            name: 'min line on close',
                            type: 'min',
                            valueDim: 'close'
                        },
                        {
                            name: 'max line on close',
                            type: 'max',
                            valueDim: 'close'
                        }
                    ]
                },
            },
            {
                name: 'MA5',
                type: 'line',
                data: calculateMA(5, data.candlestickData),
                smooth: true,
                lineStyle: {
                    opacity: 0.5
                }
            },
            {
                name: 'MA10',
                type: 'line',
                data: calculateMA(10, data.candlestickData),
                smooth: true,
                lineStyle: {
                    opacity: 0.5
                }
            },
            {
                name: 'MA20',
                type: 'line',
                data: calculateMA(20, data.candlestickData),
                smooth: true,
                lineStyle: {
                    opacity: 0.5
                }
            },
            {
                name: 'MA30',
                type: 'line',
                data: calculateMA(30, data.candlestickData),
                smooth: true,
                lineStyle: {
                    opacity: 0.5
                }
            },
            {
                name: 'Volume',
                type: 'bar',
                xAxisIndex: 1,
                yAxisIndex: 1,
                data: data.volumeData
            }
        ]
    };

    return option;
}

const StaticAlpha: React.FC = () => {
    //
    const [data, setData] = useState<ChartOption>({title: '######', categoryData: [], candlestickData: [[]], volumeData: [[]], markPointData: []});

    //
    const chartRef = useRef<HTMLDivElement|null>(null);

    //
    const theChart = useRef<ECharts|null>(null)

    useEffect(() => {
        if(!chartRef?.current) {
            chartRef.current = document.getElementById('chart') as HTMLDivElement;
        }

        // 基于准备好的dom，初始化echarts实例
        theChart.current = echarts.init(chartRef.current);
    },[]);

    useEffect(() => {
        // 绘制图表
        theChart.current?.setOption(getOption(data), true);

        // 选中某区间
        theChart.current?.dispatchAction({
            type: 'brush',
            areas: [
                {
                    brushType: 'lineX',
                    coordRange: ['2020-07-01', '2020-08-20'],
                    xAxisIndex: 0
                }
            ]
        });
    },[data]);
  
    return (
        <PageContainer>
            <Card>
                <Space>
                    <Button type="primary" 
                        icon={<PlayCircleOutlined />} 
                        onClick={async (value) => {
                 
                            const [err, data] = await handleStock();
                            if(data) {
                                console.info("success");
                                setData(splitData(data));
                            } else {
                                console.debug(err);
                            }
 
                    }}>Start</Button>
                </Space>
            </Card>
            <Card>
                <div ref={chartRef} id="chart" style={{ width: 1600, height: 700 }}></div>
            </Card>
        </PageContainer>
    );
};

export default StaticAlpha;