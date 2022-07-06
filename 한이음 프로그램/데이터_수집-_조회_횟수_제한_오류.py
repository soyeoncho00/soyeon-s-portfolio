import sys
import datetime
import time
import os
 
# pyqt
from PyQt5.QtWidgets import *
from PyQt5 import uic
from PyQt5.QAxContainer import *
 
# csv
import csv
form_class = uic.loadUiType("main_window.ui")[0]

class MyWindow(QMainWindow, form_class):
    def __init__(self):
        super().__init__()
        self.kiwoom = QAxWidget("KHOPENAPI.KHOpenAPICtrl.1")    # 키움 Open API사용
        self.login()
        self.setUI()
 
        # 변수 선언
        self.repeatNum = 0
        self.input_data = []
        self.dicStock = {}
 
        # 저장할 폴더 없으면 생성
        dirname = './Data'
        if not os.path.isdir(dirname):
            os.mkdir(dirname)
 
    def login(self):
        self.kiwoom.dynamicCall("CommConnect()")
 
    def setUI(self):
        self.setupUi(self)
 
        # 키움 Open API Trigger
        self.kiwoom.OnReceiveTrData.connect(self.receive_trdata)
        self.kiwoom.OnEventConnect.connect(self.eventConnect)
 
        # Qt Trigger
        self.pushButton.clicked.connect(self.getDatas)
 
    def eventConnect(self, nErrCode):
        if nErrCode == 0:
            print("로그인 성공")
            self.getCodeListByMarket()
        elif nErrCode == 100:
            print("사용자 정보교환 실패")
        elif nErrCode == 101:
            print("서버접속 실패")
        elif nErrCode == 102:
            print("버전처리 실패")
 
    # 종목리스트 가져와서 저장
    def getCodeListByMarket(self):
        codeList = self.kiwoom.dynamicCall("GetCodeListByMarket(QString)", "0").split(";")
        
        # count = len(codeList)
        # print(count,"\n\n")
        ####코스닥 10넣어주면 될듯..??
        
        codeList.pop()
        for code in codeList:
            itemName = self.kiwoom.dynamicCall("GetMasterCodeName(QString)",code)
            self.dicStock[code] = itemName
        print(self.dicStock)
        
    # 주식일봉차트 조회를 위한 관리
    def getDatas(self):
        scrNo = 0           # 키움 OpenAPI의 경우에 200개의 스크린 번호를 사용할수 있고, 해당 스크린 번호가 겹치면 이상한 데이터가 섞여서 올수 있음
        checkPoint = -1     # 전에 수집한 데이터 수집은 건너뛰고 데이터 수집을 하기 위해 변수 사용
        startStock = self.textEdit.toPlainText()        # 데이터 수집을 시작할 종목이름
        for key in self.dicStock.keys():
            if self.dicStock[key] == startStock:
                checkPoint = 0
 
            if checkPoint == 0:
                scrNo += 1
                scrNo = scrNo % 199
                print(key,"수집 시작")
                for i in range(20):
                    self.getData(key, scrNo)
                    time.sleep(0.4)             # 데이터 수집후 0.4초만큼 딜레이를 줌, 키움 Open API의 경우 1초당 5회 요청할수 있다고 하는데 실제로 제약사항이 더 있음
                    if self.repeatNum == -1:    # 데이터의 마지막까지 불렀다면 그만 요청함
                        break
                self.saveData(self.dicStock[key])   # 데이터 수집후 저장 (은근 딜레이 먹음)
                time.sleep(0.8)                     # 딜레이 또 주는 중, 그래도 실질적인 제약사항 충족이 안됨
 
    # 주식일봉차트 조회 요청
    def getData(self, code, scrNo):
        if self.repeatNum != -1:
            now = datetime.datetime.now()
            nowdate = now.strftime('%Y%m%d')            # 오늘 날짜를 기준으로 데이터를 받아옴
 
            self.kiwoom.dynamicCall("SetInputValue(QString, QString)", "종목코드", str(code))
            self.kiwoom.dynamicCall("SetInputValue(QString, QString)", "조회일자", nowdate)
            self.kiwoom.dynamicCall("SetInputValue(QString, QString)", "표시구분", "0")
 
            # repeatNum는 반복번호인데 처음에는 0, 반복해서 다음 데이터를 불러올라면 trData수신시 prev_next번호를 주는데 그 값으로 다시 요청하면 됨
            self.kiwoom.dynamicCall("CommRqData(QString, QString, int, QString)" , '주식일봉차트조회요청', "opt10081", self.repeatNum, str(scrNo+6000))
        else:
            print("요청 안함", self.repeatNum)
 
    # 데이터 저장
    # 데이터가 중복되는 것이 있는데 수집할때 제대로 수집이 안되고 있긴함
    # 따라서 중복제거랑 정렬을 해줘야 하는데
    # 데이터 수집후 배치처리로 하는게 좋다고 판단
    # 따라서 여기서는 일단 데이터 저장
    def saveData(self, itemName):
        f = open('./Data/'+itemName+'.csv','w', encoding='utf-8', newline='')
        wr = csv.writer(f)
        for line in self.input_data:
            wr.writerow(line)
        f.close()
 
        print(itemName," 저장")
 
        self.input_data.clear()
        self.repeatNum = 0
 
    # 데이터 수신 후 저장
    def receive_trdata(self, screen_no, rqname, trcode, recordname, prev_next, data_len, err_code, msg1, msg2):
        if trcode == "opt10081":
            if rqname == "주식일봉차트조회요청":
                count = int(self.kiwoom.dynamicCall("GetRepeatCnt(QString, QString)", trcode, rqname))
                if count != 0:
                    for index in range(count):
                        m_date = self.kiwoom.dynamicCall("GetCommData(Qstring, QString, int, QString)", trcode, rqname,index, "일자").strip(" ")
                        openPrice = int(self.kiwoom.dynamicCall("GetCommData(Qstring, QString, int, QString)", trcode, rqname,index, "시가"))
                        highPrice = int(self.kiwoom.dynamicCall("GetCommData(Qstring, QString, int, QString)", trcode, rqname, index,"고가"))
                        lowPrice = int(self.kiwoom.dynamicCall("GetCommData(Qstring, QString, int, QString)", trcode, rqname, index,"저가"))
                        currentPrice = int(self.kiwoom.dynamicCall("GetCommData(Qstring, QString, int, QString)", trcode, rqname, index,"현재가"))
                        volumn = int(self.kiwoom.dynamicCall("GetCommData(Qstring, QString, int, QString)", trcode, rqname, index,"거래량"))
                        # tradingValue = int(self.kiwoom.dynamicCall("GetCommData(Qstring, QString, int, QString)", trcode, rqname, index,"거래대금"))
                        self.input_data.append((m_date, openPrice, highPrice, lowPrice, currentPrice, volumn))
                        # self.input_data.append((m_date, openPrice, highPrice, lowPrice, currentPrice, volumn, tradingValue))
 
                    if count < 600:
                        self.repeatNum = -1
                    else:
                        self.repeatNum = prev_next
 
 
if __name__ == "__main__":
    app = QApplication(sys.argv)
    myWindow = MyWindow()
    myWindow.show()
    app.exec_()
