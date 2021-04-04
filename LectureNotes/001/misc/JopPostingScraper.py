# Indeed Job Posting Scraper for 421

from selenium.webdriver.common.by import By
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.action_chains import ActionChains
from selenium.common.exceptions import NoSuchElementException
import numpy as np
import pandas as pd
import time
from http_request_randomizer.requests.proxy.requestProxy import RequestProxy
req_proxy = RequestProxy()  # you may get different number of proxy when  you run this at each time

proxies = req_proxy.get_proxy_list()

# specify driver path
PROXY = proxies[0].get_address()
webdriver.DesiredCapabilities.CHROME['proxy'] = {
    "httpProxy": PROXY,
    "ftpProxy": PROXY,
    "sslProxy": PROXY,

    "proxyType": "MANUAL",

}

DRIVER_PATH = '/Users/connor/Documents/chromedriver'
driver = webdriver.Chrome(executable_path=DRIVER_PATH)

driver.get("https://www.indeed.com/advanced_search")
driver.implicitly_wait(4)


# search data science, data analyst, data czar, cringe?
city_set = list(set(["New York",
                     "Chicago",
                     "San Francisco",
                     "Austin",
                     "Seattle",
                     "Los Angeles",
                     "San Diego",
                     "Las Vegas",
                     "Philadelphia",
                     "Atlanta",
                     "Dallas",
                     "Pittsburgh",
                     "Portland",
                     "Charlotte",
                     "Boston",
                     "Portland, MN"]))


titles = []
companies = []
locations = []
links = []
reviews = []
salaries = []
descriptions = []
uses_r = []
uses_python = []
uses_julia = []
uses_stata = []
uses_sas = []
uses_SQL = []
city_list = []

for city in city_set:
    PROXY = proxies[0].get_address()
    webdriver.DesiredCapabilities.CHROME['proxy'] = {
        "httpProxy": PROXY,
        "ftpProxy": PROXY,
        "sslProxy": PROXY,

        "proxyType": "MANUAL",
    }

    driver = webdriver.Chrome(executable_path=DRIVER_PATH)
    driver.get("https://www.indeed.com/advanced_search")
    initial_search_button = driver.find_element_by_xpath('//*[@id="fj"]')
    location_field = driver.find_element_by_xpath('//*[@id="where"]')
    # use CONTROL in place of COMMAND on linux/windows
    ActionChains(driver).click(on_element=location_field).key_down(Keys.COMMAND).send_keys('a').key_up(Keys.COMMAND).send_keys(Keys.BACK_SPACE).perform()
    location_field.send_keys([city])
    term_field = driver.find_element_by_xpath('//*[@id="as_and"]')
    # use CONTROL in place of COMMAND on linux/windows
    ActionChains(driver).click(on_element=term_field).key_down(Keys.COMMAND).send_keys('a').key_up(Keys.COMMAND).send_keys(Keys.BACK_SPACE).perform()
    term_field.send_keys(['data science'])
    driver.find_element_by_xpath('//*[@id="radius"]/option[7]').click()
    driver.find_element_by_xpath('//*[@id="limit"]/option[4]').click()
    term_field.send_keys(Keys.RETURN)
    try:
        close_popup = driver.find_element_by_xpath('//*[@id="popover-x"]/button')
        close_popup.click()
    except:
        pass
    for i in range(0, 5):
        try:
            close_popup = driver.find_element_by_xpath('//*[@id="popover-x"]/button')
            close_popup.click()
        except:
            pass

        job_card = driver.find_elements_by_xpath('//div[contains(@class,"clickcard")]')

        for job in job_card:
            time.sleep(1)
            city_list.append(city)

            #.  not all companies have review
            try:
                review = job.find_element_by_xpath('.//span[@class="ratingsContent"]').text
            except:
                review = "None"
            reviews.append(review)
            #.   not all positions have salary
            try:
                salary = job.find_element_by_xpath('.//span[@class="salaryText"]').text
            except:
                salary = "None"
            #.  tells only to look at the element
            salaries.append(salary)
            try:
                description = job.find_element_by_xpath('//*[@id="jobDescriptionText"]').text
            except:
                description = "None"
            descriptions.append(description)
            try:
                location = job.find_element_by_xpath('.//span[contains(@class,"location")]').text
            except:
                location = "None"
            #.  tells only to look at the element
            locations.append(location)

            try:
                title = job.find_element_by_xpath('.//h2[@class="title"]//a').text
            except:
                title = job.find_element_by_xpath('.//h2[@class="title"]//a').get_attribute(name="title")
            titles.append(title)
            links.append(job.find_element_by_xpath('.//h2[@class="title"]//a').get_attribute(name="href"))
            companies.append(job.find_element_by_xpath('.//span[@class="company"]').text)

        try:
            try:
                next_page = driver.find_element_by_xpath('//*[@id="resultsCol"]/nav/div/ul/li[7]/a')
                next_page.click()
            except:
                next_page = driver.find_element_by_xpath('//a[@aria-label={}]//span[@class="pn"]'.format(i + 2))
                next_page.click()
        except:
            break
            next_page.click()

        # except:
        # next_page = driver.find_element_by_xpath('//a[.//span[contains(text(),"Next")]]')
        # next_page.click()

        print("Page: {}".format(str(i + 2)))

# load results into data-frame
df_da = pd.DataFrame()
df_da['Title'] = titles
df_da['Company'] = companies
df_da['Location'] = "Palo Alto, CA"
df_da['Link'] = links
df_da['Review'] = reviews
df_da['Salary'] = salaries
df_da['descriptions'] = descriptions

df_da.to_csv("/Users/connor/Desktop/GithubProjects/Econometrics/EC421/Spring2021/LectureNotes/JobRelevancyData1.csv", encoding=utf - 8)

descriptions = []
i = 0
for link in links:
    i = i + 1
    if i % 10 != 0:
        proxies = req_proxy.get_proxy_list()

        # specify driver path
        PROXY = proxies[0].get_address()
        webdriver.DesiredCapabilities.CHROME['proxy'] = {
            "httpProxy": PROXY,
            "ftpProxy": PROXY,
            "sslProxy": PROXY,

            "proxyType": "MANUAL",
        }

        driver = webdriver.Chrome(executable_path=DRIVER_PATH)
        driver.implicitly_wait(8)
        driver.get(link)
        try:
            jd = driver.find_element_by_xpath('//div[@id="jobDescriptionText"]').text
        except:
            jd = "None"
        # search for R requirements
        if " r " or " r, " or " r." in jd.lower():
            R = 1
        else:
            R = 0
        # search for python req
        if "python" in jd.lower():
            python = 1
        else:
            python = 0
        # search for SQL req
        if " sql" in jd.lower():
            SQL = 1
        else:
            SQL = 0
        # search for sas req
        if " sas" in jd.lower():
            sas = 1
        else:
            sas = 0
        # search for stata req
        if "stata" in jd.lower():
            stata = 1
        else:
            stata = 0
        # search for Julia req
        if "julia" in jd.lower():
            julia = 1
        else:
            julia = 0
    else:
        time.sleep(6)
        driver.implicitly_wait(8)
        driver.get(link)
        try:
            jd = driver.find_element_by_xpath('//div[@id="jobDescriptionText"]').text
        except:
            jd = "None"
        # search for R requirements
        if " r " or " r, " or " r." in jd.lower():
            R = 1
        else:
            R = 0
        # search for python req
        if "python" in jd.lower():
            python = 1
        else:
            python = 0
        # search for SQL req
        if " sql" in jd.lower():
            SQL = 1
        else:
            SQL = 0
        # search for sas req
        if " sas" in jd.lower():
            sas = 1
        else:
            sas = 0
        # search for stata req
        if "stata" in jd.lower():
            stata = 1
        else:
            stata = 0
        # search for Julia req
        if "julia" in jd.lower():
            julia = 1
        else:
            julia = 0
    # build list of summary variables from long link-list + add descriptions to dataset
    descriptions.append(jd)
    uses_r.append(R)
    uses_julia.append(julia)
    uses_SQL.append(SQL)
    uses_sas.append(sas)
    uses_stata.append(stata)
    uses_python.append(python)

# load results into data-frame
df_da = pd.DataFrame()
df_da['Title'] = titles
df_da['Company'] = companies
df_da['Location'] = "Palo Alto, CA"
df_da['Link'] = links
df_da['Review'] = reviews
df_da['Salary'] = salaries
df_da['Description'] = descriptions
df_da['uses_julia'] = uses_julia
df_da['uses_r'] = uses_r
df_da['uses_SQL'] = uses_SQL
df_da['uses_stata'] = uses_stata
df_da['uses_sas'] = uses_sas
df_da['uses_python'] = uses_python
df_da['city'] = city_list

# write to disk
df_da.to_csv("/Users/connor/Desktop/GithubProjects/Econometrics/EC421/Spring2021/LectureNotes/JobRelevancyData.csv", encoding=utf - 8)

driver.close()
