(ns javadeps.llm
  (:require
   [clojure.data.json :as json]
   [org.httpkit.client :as http]))

(def ^:private llm-config
  {:anthropic {:url "https://api.anthropic.com/v1/messages"
               :model "claude-3-opus-20240229"
               :cost {:input 0.015, :output 0.075}
               :api-key (System/getenv "ANTHROPIC_API_KEY")}
   :ollama {:url (str (or (System/getenv "OLLAMA_HOST") "http://localhost:11434") "/api/generate")
            :model (or (System/getenv "OLLAMA_MODEL") "qwen2.5-coder:7b")}})

(defn- estimate-tokens [text]
  (int (/ (count text) 4)))

(defn call-llm
  "Call LLM API for analysis"
  [type prompt]
  (let [config (type llm-config)]
    (try
      (case type
        :anthropic @(http/post (:url config)
                      {:headers {"x-api-key" (:api-key config)
                                 "anthropic-version" "2023-06-01"
                                 "content-type" "application/json"}
                       :body (json/write-str
                               {:model (:model config)
                                :max_tokens 4096
                                :messages [{:role "user" :content prompt}]})})
        :ollama @(http/post (:url config)
                            {:headers {"Content-Type" "application/json"}
                             :body (json/write-str
                                     {:model (:model config)
                                      :stream false
                                      :prompt prompt})}))
      (catch Exception e
        (println "Failed to connect to" (name type) ":" (.getMessage e))
        nil))))

(def ^:private refactoring-prompt
  "Analyze this Java project dependency graph and identify the top 3 most important, concrete refactoring improvements. For each:
1. Identify specific classes and their problematic dependency patterns
2. Explain why it's a problem (e.g., tight coupling, circular dependencies)
3. Suggest a specific, actionable solution

Focus only on the most critical issues that would give the biggest improvement in maintainability.

Dependency graph:

%s")

(defn get-refactoring-advice
  "Get refactoring advice using specified LLM"
  [dep-data llm]
  (let [prompt (format refactoring-prompt dep-data)
        llm-type (keyword llm)]
    (when-let [response (call-llm llm-type prompt)]
      (when (= 200 (:status response))
        (let [advice (case llm-type
                       :anthropic (get-in (json/read-str (:body response)) ["content" 0 "text"])
                       :ollama (get (json/read-str (:body response)) "response"))]
          (cond-> {:advice advice}
            (= :anthropic llm-type)
            (assoc :cost
              (let [input-tokens (estimate-tokens prompt)
                    output-tokens (estimate-tokens advice)
                    costs (get-in llm-config [:anthropic :cost])]
                {:input-tokens input-tokens
                 :output-tokens output-tokens
                 :total-cost (+ (* (:input costs) (/ input-tokens 1000))
                               (* (:output costs) (/ output-tokens 1000)))}))))))))
