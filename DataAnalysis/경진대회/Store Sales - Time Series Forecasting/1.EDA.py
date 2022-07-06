#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


import calendar

import plotly.express as px
from plotly.subplots import make_subplots
import plotly.figure_factory as ff
import plotly.offline as offline
import plotly.graph_objs as go
offline.init_notebook_mode(connected = True)


# In[3]:


import os
print(os.listdir('./data'))


# In[4]:


train = pd.read_csv ("./data/train.csv")
test = pd.read_csv ("./data/test.csv")
oil = pd.read_csv ("./data/oil.csv")
stores = pd.read_csv ("./data/stores.csv") 
trans = pd.read_csv ("./data/transactions.csv")  
holi = pd.read_csv ("./data/holidays_events.csv")   


# In[5]:


train


# In[6]:


train1 = train.merge(holi, on = 'date', how='left')
train1 = train1.merge(oil, on = 'date', how='left')
train1 = train1.merge(stores, on = 'store_nbr', how='left')
train1 = train1.merge(trans, on = ['date', 'store_nbr'], how='left')


# In[7]:


train1.info()


# In[8]:


train1.pop('id')


# #### 데이터끼리 합치기

# In[9]:


train1 = train1.rename(columns = {"type_x" : "holiday_type", "type_y" : "store_type"})
train1


# ####  Pandas 일자와 시간(dt) 처리법
# > dt.year : 연(4자리숫자)
# 
# > dt.month : 월(숫자)
# 
# > dt.isocalendar().week : 그 해의 몇주인지 알고 싶을 때
# 
# > dt.quarter : 분기(숫자)
# 
# > dt.day_name() : 일(문자)

# In[10]:


train1['date'] = pd.to_datetime(train1['date'])
train1['year'] = train1['date'].dt.year
train1['month'] = train1['date'].dt.month 
train1['week'] = train1['date'].dt.isocalendar().week
train1['quarter'] = train1['date'].dt.quarter 
train1['day_of_week'] = train1['date'].dt.day_name() 
train1


# In[11]:


train1.info()


# In[12]:


# train1['week'].value_counts(ascending=True)


# In[13]:


df_year
x_data[0]


# ### 년도별 달마다 sales증가량(평균값) 변화 정도 시각화

# In[ ]:


df_2013 = train1[train1['year']==2013][['month','sales']]
df_2013 = df_2013.groupby('month').agg({"sales" : "mean"}).reset_index().rename(columns={'sales':'s13'})
df_2014 = train1[train1['year']==2014][['month','sales']]
df_2014 = df_2014.groupby('month').agg({"sales" : "mean"}).reset_index().rename(columns={'sales':'s14'})
df_2015 = train1[train1['year']==2015][['month','sales']]
df_2015 = df_2015.groupby('month').agg({"sales" : "mean"}).reset_index().rename(columns={'sales':'s15'})
df_2016 = train1[train1['year']==2016][['month','sales']]
df_2016 = df_2016.groupby('month').agg({"sales" : "mean"}).reset_index().rename(columns={'sales':'s16'})
df_2017 = train1[train1['year']==2017][['month','sales']]
df_2017 = df_2017.groupby('month').agg({"sales" : "mean"}).reset_index()
df_2017_no = pd.DataFrame({'month': [9,10,11,12], 'sales':[0,0,0,0]})
df_2017 = df_2017.append(df_2017_no).rename(columns={'sales':'s17'})
df_year = df_2013.merge(df_2014,on='month').merge(df_2015,on='month').merge(df_2016,on='month').merge(df_2017,on='month')

top_labels = ['2013', '2014', '2015', '2016', '2017']

colors = ['rgba(38, 24, 74, 0.8)', 'rgba(71, 58, 131, 0.8)',
          'rgba(122, 120, 168, 0.8)', 'rgba(164, 163, 204, 0.85)',
          'rgba(190, 192, 213, 1)']
df_year = df_year[['s13','s14','s15','s16','s17']].replace(np.nan,0) #판다스의 경우 NaN아 아닌 np.nan을 이용
x_data = df_year.values

df_2013['month'] =['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
y_data = df_2013['month'].tolist()

fig = go.Figure()
for i in range(0, len(x_data[0])):
    for xd, yd in zip(x_data, y_data):
        fig.add_trace(go.Bar(
            x=[xd[i]], y=[yd],
            orientation='h',
            marker=dict(
                color=colors[i],
                line=dict(color='rgb(248, 248, 249)', width=1) #색 결정하는 거 같음
            )
        ))

fig.update_layout(title='Avg Sales for each Year',
    xaxis=dict(showgrid=False, 
               zeroline=False, domain=[0.15, 1]),
    yaxis=dict(showgrid=False, showline=False,
               showticklabels=False, zeroline=False),
    barmode='stack', 
    template="plotly_white",
    margin=dict(l=0, r=50, t=100, b=10),
    showlegend=False, 
)

annotations = []
for yd, xd in zip(y_data, x_data):
    # labeling the y-axis
    annotations.append(dict(xref='paper', yref='y',
                            x=0.14, y=yd,
                            xanchor='right',
                            text=str(yd),
                            font=dict(family='Arial', size=14,
                                      color='rgb(67, 67, 67)'),
                            showarrow=False, align='right'))
    # labeling the first Likert scale (on the top)
    if yd == y_data[-1]:
        annotations.append(dict(xref='x', yref='paper',
                                x=xd[0] / 2, y=1.1,
                                text=top_labels[0],
                                font=dict(family='Arial', size=14,
                                          color='rgb(67, 67, 67)'),
                          showarrow=False))
    space = xd[0]
    for i in range(1, len(xd)):
            # labeling the Likert scale
            if yd == y_data[-1]:
                annotations.append(dict(xref='x', yref='paper',
                                        x=space + (xd[i]/2), y=1.1,
                                        text=top_labels[i],
                                        font=dict(family='Arial', size=14,
                                                  color='rgb(67, 67, 67)'),
                                        showarrow=False))
            space += xd[i]
fig.update_layout(
    annotations=annotations)
fig.show()


# #### 함수 설명
# > 1.groupby('month').agg({"sales" : "mean"}) : months로 groupby된 train1 data, 각 월별 sales data의 mean 값들끼리 비교
# 
# > 2. round(2) :  소수 둘째자리까지 반올림 
# ![image-2.png](attachment:image-2.png)  ![image-3.png](attachment:image-3.png)
# 
# > 3. calendar: 파이썬에서 달력을 볼 수 있게 해주는 모듈
# - calendar.month_abbr :현재 로케일에서 연중 약식 월을 나타내는 배열. 이는 1월이 월 번호 1인 일반적인 규칙을 따르므로, 길이는 13이고 month_abbr[0]은 빈 문자열입니다
#  
# > 4. apply : 함수를 적용해줌 해당 열이나 행
# 
# > 5. astype: df.astype(dtype, copy=True, errors='raies')//concat할 때 같은 dtype이 아니어서 astype으로 데이터 타입 변환 해줘야함
# ![image-5.png](attachment:image-5.png)
# ![image-4.png](attachment:image-4.png)
# 

# In[ ]:


#df_m_sa['month_text']
#df_w_sa


# In[ ]:


df_m_sa = train1.groupby('month').agg({"sales" : "mean"}).reset_index() 
df_m_sa['sales'] = round(df_m_sa['sales'],2) 
df_m_sa['month_text'] = df_m_sa['month'].apply(lambda x: calendar.month_abbr[x]) 
df_m_sa['text'] = df_m_sa['month_text'] + '-' + df_m_sa['sales'].astype(str)

df_w_sa = train1.groupby('week').agg({"sales" : "mean"}).reset_index() 
df_q_sa = train1.groupby('quarter').agg({"sales" : "mean"}).reset_index()
df_m_sa['color'] = '#496595'
df_m_sa['color'][:-1] = '#c6ccd8'
df_w_sa['color'] = '#c6ccd8'

# chart
fig = make_subplots(rows=2, cols=2, vertical_spacing=0.08,
                    row_heights=[0.7, 0.3], 
                    specs=[[{"type": "bar"}, {"type": "pie"}],
                           [{"colspan": 2}, None]],
                    column_widths=[0.7, 0.3],
                    subplot_titles=("Month wise Avg Sales Analysis", "Quarter wise Avg Sales Analysis", 
                                    "Week wise Avg Sales Analysis"))

fig.add_trace(go.Bar(x=df_m_sa['sales'], y=df_m_sa['month'], marker=dict(color= df_m_sa['color']),
                     text=df_m_sa['text'],textposition='auto',
                     name='Month', orientation='h'), 
                     row=1, col=1)
fig.add_trace(go.Pie(values=df_q_sa['sales'], labels=df_q_sa['quarter'], name='Quarter',
                     marker=dict(colors=['#334668','#496595','#6D83AA','#91A2BF','#C8D0DF']), hole=0.7,
                     hoverinfo='label+percent+value', textinfo='label+percent'), 
                     row=1, col=2)
fig.add_trace(go.Scatter(x=df_w_sa['week'], y=df_w_sa['sales'], mode='lines+markers', fill='tozeroy', fillcolor='#c6ccd8',
                     marker=dict(color= '#496595'), name='Week'), 
                     row=2, col=1)

# styling
fig.update_yaxes(visible=False, row=1, col=1)
fig.update_xaxes(visible=False, row=1, col=1)
fig.update_xaxes(tickmode = 'array', tickvals=df_w_sa.week, ticktext=[i for i in range(1,53)], 
                 row=2, col=1)
fig.update_yaxes(visible=False, row=2, col=1)
fig.update_layout(height=750, bargap=0.15,
                  margin=dict(b=0,r=20,l=20), 
                  title_text="Average Sales Analysis",
                  template="plotly_white",
                  title_font=dict(size=25, color='#8a8d93', family="Lato, sans-serif"),
                  font=dict(color='#8a8d93'),
                  hoverlabel=dict(bgcolor="#f2f2f2", font_size=13, font_family="Lato, sans-serif"),
                  showlegend=False)
fig.show()


# In[ ]:


uniq = np.unique(train['date'].astype(str))
print(uniq)


# In[ ]:


train['date'].value_counts()


# In[ ]:


train_2013 = train[train['date'].str.contains('2013')]
train_2013


# ## sales 와 시간에 따라 관계가 있는지 시계열성 확인
# ### 내가 하고싶은 것
# - 우선 데이터가 너무 많으니까 각 년도 별로 sales가 시간에 따라 관계에 있는지(월별로) 대부분의 년도에서 같은 흐름을 보이는 지 확인하기

# In[ ]:


train_data_2013 = train_2013['date']
pd.DataFra


# In[ ]:


import matplotlib
import matplotlib.pyplot as plt
get_ipython().run_line_magic('matplotlib', 'inline')
import seaborn as sns
f, ax = plt.subplots(figsize=(10,5))
#seaborn을 사용한 막대 그래프를 생성한다.
sns.countplot(x=train_date_2013, data=train_sales_2013, alpha=0.5)
#show() 함수를 통해 시각화 한다.
plt.show()


# In[ ]:


type(train)


# In[ ]:


train.isnull().sum() #null값 존재하는지 보기


# In[ ]:





# In[ ]:


oil.bfill(inplace=True)


# In[ ]:


plt.plot(train['sales'])
plt.show()


# In[ ]:


train = train['family']


# In[ ]:


test


# In[ ]:




