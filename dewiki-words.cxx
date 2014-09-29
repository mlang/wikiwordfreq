#include <cctype>
#include <chrono>
#include <condition_variable>
#include <deque>
#include <fstream>
#include <iostream>
#include <locale>
#include <mutex>
#include <queue>
#include <regex>
#include <string>
#include <thread>
#include <unordered_map>

#include <xml/parser>

#include <boost/algorithm/string/replace.hpp>
#include <boost/locale/encoding_utf.hpp>
#include <boost/tokenizer.hpp>

typedef std::unordered_map<std::string, size_t> word_map_type;
struct : word_map_type {
  std::mutex mutex;
} word_map;

word_map_type& operator+=(word_map_type &lhs, word_map_type &&rhs) {
  for (auto &&pair: rhs) lhs[std::move(pair.first)] += pair.second;
  return lhs;
}

struct : std::queue<std::string> {
  std::mutex mutex;
  std::condition_variable changed;
  bool done = false;
} wq;

void worker() {
  using namespace std;
  using namespace boost;

  unique_lock<decltype(wq.mutex)> queue_lock(wq.mutex);

  while (true) {
    if (not wq.empty()) {
      string text { move(wq.front()) };
      wq.pop();
      queue_lock.unlock();
      wq.changed.notify_one();

      replace_all(text, "align=\"right\"", "");
      replace_all(text, "align=\"center\"", "");
      replace_all(text, "align=\"left\"", "");
      replace_all(text, "valign=\"top\"", "");
      replace_all(text, "class = \"wikitable\"", "");
      replace_all(text, "style = \"text-align: center\"", "");
      replace_all(text, "<br />", "\n");
      replace_all(text, "<br style=\"clear:left\"/>", "\n");
      replace_all(text, "<br clear=\"both\"/>", "\n");
      replace_all(text, "&nbsp;", " ");
      replace_all(text, "<references />", "");
      replace_all(text, "<nowiki />", "");

      static pair<regex, string> const patterns[] = {
        { regex("<!--(.|\n)*?-->"
  	    "|" "<math>.*?</math>"
	    "|" "\\[\\[Datei:.*?\\]\\]"
	    "|" "\\{\\{.*?\\}\\}"), "" },
        { regex("\\[https?://[^ ]* (.*?)\\]"), "$1" },
        { regex("\\[\\[Kategorie:(\\w*)\\]\\]"), "$1" },
        { regex("\\[\\[Kategorie:(\\w*)\\|.*\\]\\]"), "$1" },
        { regex("\\[\\[(.*?)\\]\\]"), "$1" },
        { regex("<small>(.*?)</small>"), "$1" },
        { regex("<sub>(.*?)</sub>"), "$1" },
        { regex("<sup>(.*?)</sup>"), "$1" },
        { regex("<tt>(.*?)</tt>"), "$1" },
        { regex("<u>(.*?)</u>"), "$1" },
        { regex("<ref(>| [^>]*>)(.*?)</ref>"), " $2 " },
        { regex("<ref name=\"([^\"]*)\" ?/>"), " $1 " }
      };

      for (auto &&pat: patterns) {
        text = regex_replace(text, pat.first, pat.second);
      }

      {
        word_map_type words;
        for (auto &&word: tokenizer<>(text)) {
          wstring const wword { locale::conv::utf_to_utf<wstring::value_type>(word) };
          if (wword.size() > 1 and
              all_of(wword.begin(), wword.end(), static_cast<int(*)(wint_t)>(iswalpha)))
            words[move(word)]++;
        }

        lock_guard<decltype(word_map.mutex)> guard{word_map.mutex};
        word_map += move(words);
      }

      queue_lock.lock();
    } else if (wq.done) {
      break;
    } else {
      wq.changed.wait(queue_lock);
    }
  }
}

int
main (int argc, char* argv[])
{
  using namespace std;
  using namespace std::literals::chrono_literals;

  size_t articles { 0 };

  size_t chars_in { 0 };

  locale::global(locale(""));

  try {
    cin.exceptions(ifstream::badbit | ifstream::failbit);

    xml::parser p(cin, "<STDIN>",
                  xml::parser::receive_default |
                  xml::parser::receive_attributes_event);

    xml::qname current_element_qname;
    bool inside_attribute = false, redirect = false;
    auto start_time = chrono::steady_clock::now();
    auto last_output = chrono::steady_clock::now();

    vector<thread> threads;
    
    for (auto event = p.next(); event != xml::parser::eof; event = p.next()) {
      switch (event) {
      default:
        break;
      case xml::parser::start_element:
        current_element_qname = p.qname();
        if (p.name() == "redirect") redirect = true;
	break;
      case xml::parser::end_element: {
        current_element_qname = xml::qname();
        if (p.name() == "page") {
	  articles++;
	  redirect = false;

          auto current_time = chrono::steady_clock::now();
          if ((current_time - last_output) > 1s) {
            last_output = current_time;
	    auto cps = chars_in / ((current_time - start_time) / 1s);
	    {
	      unique_lock<decltype(word_map.mutex)> word_map_lock(word_map.mutex);
              word_map_type::size_type const unique_words { word_map.size() };
              word_map_lock.unlock();

	      clog << cps << " cps, "
		   << articles << " pages, "
                   << unique_words << " unique words found\r";
	    }
          }
        }
        break;
      }
      case xml::parser::start_attribute:
        inside_attribute = true;
        break;
      case xml::parser::end_attribute:
        inside_attribute = false;
        break;
      case xml::parser::characters:
        if (not redirect and not inside_attribute and
            (current_element_qname.name() == "text" or
             current_element_qname.name() == "title")) {
          stringstream stream(p.value());
          while (p.peek() == xml::parser::characters) {
            p.next();
            stream << p.value();
          }
	  string text { stream.str() };
	  chars_in += text.size();

	  { // push work to queue
	    unique_lock<decltype(wq.mutex)> lock{wq.mutex};
	    wq.changed.wait(lock, [] {
              return wq.size() < (thread::hardware_concurrency() << 8);
            });
	    wq.push(move(text));
	  }
          wq.changed.notify_one();
        }
        break;
      }

      if (threads.size() < thread::hardware_concurrency()) {
        unique_lock<decltype(wq.mutex)> lock{wq.mutex};
        if (wq.size() > (thread::hardware_concurrency() << 7)) {
	  lock.unlock();
	  threads.emplace_back(worker);
        }
      }

      if (articles == 1000000) break;
    }

    {
      lock_guard<decltype(wq.mutex)> guard{wq.mutex};
      wq.done = true;
    }
    wq.changed.notify_all();
    for (auto &&thread: threads) thread.join();
  } catch (const ios_base::failure&) {
    cerr << "io failure" << endl;
    return EXIT_FAILURE;
  } catch (const xml::exception& e) {
    cerr << e.what() << endl;
    return EXIT_FAILURE;
  }

  for (auto &&pair: word_map)
    if (pair.second > 1) cout << pair.second << " " << pair.first << '\n';

  return EXIT_SUCCESS;
}
